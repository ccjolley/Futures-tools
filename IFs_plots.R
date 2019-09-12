library(tidyverse)

###############################################################################
# Define USAID themes & colors
# TODO: these aren't specific to IFs, and could probably live in a different
# file, in case I need them separately from IFs.
###############################################################################

theme_USAID <- theme( 
  axis.ticks = element_blank(), 
  axis.ticks.length = unit(0, units = "points"), 
  
  strip.text = element_blank(),
  strip.background = element_blank(),
  
  panel.border = element_blank(), 
  panel.grid = element_blank(), 
  panel.background = element_blank(), 
  plot.background = element_blank(),
  legend.key = element_blank(),
  text=element_text(family="Liberation Sans")) 

approved_colors <-c(
  '#002F6C', # USAID blue
  '#BA0C2F', # USAID red
  '#6C6463', # dark gray
  '#A7C6ED', # light blue
  '#651D32', # dark red
  '#CFCDC9'  # light gray
) 

colors_USAID <- scale_color_manual(values=approved_colors)
fill_USAID <- scale_fill_manual(values=approved_colors)

###############################################################################
# Utility functions that get re-used by a lot of plots
###############################################################################
### Draw a vertical line to distinguish history and forecast
### TODO: is there a way to extract the limits of the y-axis from the ggplot 
### object so that we can set a reasonable default for label_height?
hf_line <- function(label_height,cutoff=2014.5,split=0.25,text_orient='horizontal') {
  line <- geom_vline(xintercept=2014.5,color='black',linetype=2)
  if (text_orient == 'horizontal') {
    list(line,
         annotate('text',y=label_height,x=cutoff,hjust=1+split,label='History'),
         annotate('text',y=label_height,x=cutoff,hjust=-split,label='Forecast'))
  } else if (text_orient == 'vertical') {
    list(line,
         annotate('text',y=label_height,x=cutoff,vjust=-split,angle=90,label='History'),
         annotate('text',y=label_height,x=cutoff,vjust=1+split,angle=90,label='Forecast'))
  }
}

### Get labels for years on the x axis, making sure that we label the beginning
### of the forecast period (typically 2015)
get_breaks <- function(limits,numbreaks=6,fixed=2015,numiter=2000) {
  lo <- ceiling(min(limits)/5)*5
  hi <- floor(max(limits)/5)*5
  if (length(fixed)+2 > numbreaks) {
    message('get_breaks() passed ',length(fixed),' fixed points in addition to min and max. Please increase numbreaks')
    c(lo,hi,fixed) %>% sort %>% return
  }
  # Randomly try out numiter combinations of year labels, keep the one that
  # has the year labels most evenly-spaced. This tends to look OK.
  yrs <- c(lo,sort(fixed),hi)
  add_num <- numbreaks - 2 - length(fixed)
  possible <- seq(lo,hi,by=5) %>% setdiff(yrs)
  score <- function(x) {
    sd(x - lag(x),na.rm=TRUE)
  }
  best_score <- 1000
  best_yrs <- yrs
  for (i in 1:numiter) {
    x <- c(sample(possible,add_num),yrs) %>% sort
    s <- score(x)
    if (s < best_score) {
      best_score <- s
      best_yrs <- x
    }
  }
  best_yrs
}

### Utility function that uses get_breaks() to label years on the
### x-axis
x_year <- function(numbreaks=6,fixed=2015,numiter=2000) { 
  tmp <- function(limits) {
    get_breaks(limits,numbreaks,fixed,numiter)
  }
  list(scale_x_continuous(breaks=tmp),
       xlab('Year')
  )
}

### Put a percent scale on the y-axis. If you need to round to a different
### level of accuracy, pass a power of 10 (e.g. 0.1, 0.001) into the 
### accuracy argument.
y_percent <- function(accuracy=1) {
  scale_y_continuous(labels=scales::percent_format(accuracy=accuracy))
}

### Shared features across plots
### TODO: more elegant way to pass standard set of argument with ellipsis?
line_plot <- function(valpercent=FALSE,dots=FALSE) {
  out <- list(geom_line(size=1),
              xlab('Year'),
              theme_USAID,
              colors_USAID)
  if (valpercent) { 
    x <- append(x,scale_y_continuous(labels=scales::percent_format(accuracy=1))) 
  }
  if (dots) { out <- append(out,geom_point(size=2)) }
  out
}


###############################################################################
# Rename countries and groups
# TODO: give this function a new name to make it clear that it's cleaning up
# IFs names; check if any others are funny.
###############################################################################
geo_rename <- function(x) {
  x %>%
    sub('WB LowMidIncome Economies','LMIC',.) %>%
    sub('WB UpMidIncome Economies','UMIC',.) %>%
    sub('WB Low-Income Economies','Low-Income',.) %>%
    sub('AfrCul-West','West Africa',.) %>%
    sub('Cote dIvoire','Cote d\'Ivoire',.)
}

###############################################################################
# Country-comparison plot
# TODO: factor out a single function to handle shared features
###############################################################################
country_compare <- function(fname,ytitle,cutoff=2014.5,label_split=0.2,
                            label_height=0.9,text_orient='horizontal',
                            valpercent=FALSE,dots=FALSE,ontop=NULL) {
  ifs_df <- read_tsv(fname, col_names = c('varstr','year','value')) %>%
    na.omit %>%
    mutate(Country = sub('^.*\\(','',varstr) %>% sub(',.*?\\)','',.) %>% sub('\\)','',.),
           Country = geo_rename(Country),
           Country = fct_reorder(Country,value,.desc=TRUE)) %>%
    select(Country,year,value)
  if (valpercent) { ifs_df <- mutate(ifs_df,value=value/100)}
  
  p <- ggplot(data=ifs_df,aes(x=year,y=value,group=Country,color=Country)) +
    line_plot(valpercent,dots) +
    ylab(ytitle) 
  if (max(ifs_df$year) > cutoff & min(ifs_df$year) < cutoff) {
    lh <- min(ifs_df$value) + label_height*(max(ifs_df$value) - min(ifs_df$value))
    p <- p + hf_line(lh,split=label_split,text_orient=text_orient)
  }
  if (!is.null(ontop)) {
    p <- p + geom_line(data=ifs_df[ifs_df$Country==ontop,],size=1)
  }
  p
}

## TEST CODE
#country_compare('../Kenya/IFs_exports/life_exp.txt','Life expectancy (years)')

###############################################################################
# Scenario-comparison plot
# TODO: Rather than hard-coding scenario names in this function, I need to be
# able to pass in a "recode" data frame and join it to ifs_df to rename the
# scenarios. Also get rid of base_label argument.
###############################################################################
scenario_compare <- function(fname,ytitle,cutoff=2014.5,label_split=0.2,
                             label_height=0.9,text_orient='horizontal',
                             valpercent=FALSE,dots=FALSE,ontop=NULL,
                             base_label='Base') {
  ifs_df <- read_tsv(fname, col_names = c('varstr','year','value')) %>%
    na.omit %>%
    mutate(Scenario = sub('^.*\\[','',varstr) %>% sub('\\].*','',.),
           Scenario = ifelse(Scenario=='Sustain','Sustainability',Scenario),
           Scenario = ifelse(Scenario=='Ghana_GBA','Ghana Beyond Aid',Scenario),
           Scenario = ifelse(Scenario=='Ghana_Ag','Ag transformation',Scenario),
           Scenario = ifelse(Scenario=='Ghana_ST','S&T transformation',Scenario),
           Scenario = ifelse(Scenario==base_label,'Current path',Scenario),
           Scenario = fct_reorder(Scenario,value,.desc=TRUE)) %>%
    select(Scenario,year,value)
  if (valpercent) { ifs_df <- mutate(ifs_df,value=value/100)}

  p <- ggplot(data=ifs_df,aes(x=year,y=value,group=Scenario,color=Scenario)) +
    line_plot(valpercent,dots) +
    ylab(ytitle) 
  if (max(ifs_df$year) > cutoff & min(ifs_df$year) < cutoff) {
    lh <- min(ifs_df$value) + label_height*(max(ifs_df$value) - min(ifs_df$value))
    p <- p + hf_line(lh,split=label_split,text_orient=text_orient)
  }
  if (!is.null(ontop)) {
    p <- p + geom_line(data=ifs_df[ifs_df$Scenario==ontop,],size=1)
  }
  p
}

### TEST CODE
#scenario_compare('../Ghana/IFs_exports/final2-aid.txt','Billion dollars')

################################################################################
# Comparison of multiple countries across multiple scenarios, encoding 
# countries with colors and scenarios with line-types (or vice versa)
# TODO: add this scenario_rename feature to scenario_compare()
# TODO: ideally, I'd like for the base-case scenario to be the solid line (and 
#       on top), even if it doesn't come first alphabetically
################################################################################
country_scenario_compare <- function(fname,ytitle,cutoff=2014.5,label_split=0.2,
                                     label_height=0.9,text_orient='horizontal',
                                     valpercent=FALSE,dots=FALSE,ontop_country=NULL,
                                     color_by='Country',scenario_rename=NULL) {
  if (color_by=='Country') { 
    line_by <- 'Scenario' 
  } else {
    line_by <- 'Country'
  }
  ifs_df <- read_tsv('country_scenario_test.txt',col_names=c('varstr','year','value')) %>%
    na.omit %>%
    mutate(Country=sub('.*\\(','',varstr),
           Country=sub('\\).*','',Country),
           scenario_old=sub('.*\\[','',varstr),
           scenario_old=sub('\\].*','',scenario_old))
  if (!is.null(scenario_rename)) {
    ifs_df <- left_join(ifs_df,scenario_rename,by='scenario_old')
  } else {
    ifs_df <- rename(ifs_df,Scenario=scenario_old)
  }
  if (valpercent) { ifs_df <- mutate(ifs_df,value=value/100)}
  
  p <- ggplot(ifs_df,aes_string(x='year',y='value',group='varstr',
                           color=color_by,linetype=line_by)) +
    line_plot(valpercent,dots) +
    ylab(ytitle) 
  if (max(ifs_df$year) > cutoff & min(ifs_df$year) < cutoff) {
    lh <- min(ifs_df$value) + label_height*(max(ifs_df$value) - min(ifs_df$value))
    p <- p + hf_line(lh,split=label_split,text_orient=text_orient)
  }
  if (!is.null(ontop_country)) {
    p <- p + geom_line(data=ifs_df[ifs_df$Country==ontop_country,],size=1)
  }
  p
}

### TEST CODE
# rename_tbl <- tibble(scenario_old=c('Base','Security'),
#                      Scenario=c('Current path','Security first'))
# country_scenario_compare('country_scenario_test.txt','GDP per capita (PPP)')



### TODO: other plots it might be good to have

# cross-sectional scatterplot, drawing from two different IFs export files and
# with a specific country (or set of countries) highlighted
# use ggrepel to label either all points, or only those that are highlighted or have extreme values

# two-variable "rainbow trajectories" (like in first Ghana report)

# bubble plots (a la Malawi)

# epidemiological transition -- could do a few things, including single-country
# trajectory, scatterplot of predicted transition years (and burden levels),
# overlaying trajectory of one country on the global current state (with 
# equality line marked)

# GDP value-add -- could do something similar to epidemiological transition, 
# focused instead on the transition from agriculture to another dominant sector
# (services or manufacturing)



