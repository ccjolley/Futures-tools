library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)
library(readr)

# TODO: just load tidyverse, instead of all the individual pieces

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
  '#BA0C2F',  # USAID red
  '#6C6463',   # dark gray
  '#A7C6ED', # light blue
  '#651D32', # dark red
  '#CFCDC9' # light gray
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
  }
  if (text_orient == 'vertical') {
    list(line,
         annotate('text',y=label_height,x=cutoff,vjust=-split,angle=90,label='History') +
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
###############################################################################
country_compare <- function(fname,ytitle,dots=TRUE,label_split=0.2, 
                            label_height=0.9, ontop=NULL, 
                            text_orient='horizontal') {
  ifs_df <- read_tsv(fname, col_names = c('varstr','year','value')) %>%
    na.omit %>%
    mutate(Country = sub('^.*\\(','',varstr) %>% sub(',.*?\\)','',.) %>% sub('\\)'2,'',.),
           Country = geo_rename(Country),
           Country = fct_reorder(Country,value,.desc=TRUE)) %>%
    select(Country,year,value)
  
  p <- ggplot(data=ifs_df,aes(x=year,y=value,group=Country,color=Country)) +
    geom_line(size=1) +
    xlab('Year') +
    ylab(ytitle) +
    theme_USAID + colors_USAID
  if (max(ifs_df$year) > cutoff & min(ifs_df$year) < cutoff) {
    lh <- min(ifs_df$value) + label_height*(max(ifs_df$value) - min(ifs_df$value))
    p <- p + hf_line(lh,split=label_split,text_orient=text_orient)
  }
  if (dots) { p <- p + geom_point(size=2)}
  if (!is.null(ontop)) {
    p <- p + geom_line(data=ifs_df[ifs_df$Country==ontop,],size=1)
  }
  p
}

## TODO: make sure that country_compare and scenario_compare have matching
# argument lists and follow a parallel structure to the extent possible.

###############################################################################
# Scenario-comparison plot
# TODO: Rather than hard-coding scenario names in this function, I need to be
# able to pass in a "recode" data frame and join it to ifs_df to rename the
# scenarios. Also get rid of base_label argument.
###############################################################################
scenario_compare <- function(fname,ytitle,cutoff=2014.5,text_orient='horizontal',
                             base_label='Base',valpercent=FALSE,label_height=0.9,dots=FALSE,
                             ontop=NULL) {
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
  label_height <- min(ifs_df$value) + lh*(max(ifs_df$value) - min(ifs_df$value))
  p <- ggplot(data=ifs_df,aes(x=year,y=value,group=Scenario,color=Scenario)) +
    geom_line(size=1) +
    xlab('Year') +
    ylab(ytitle) +
    theme_USAID + colors_USAID 
  if (valpercent) { p <- p + scale_y_continuous(labels=scales::percent_format(accuracy=1)) }
  if (max(ifs_df$year) > cutoff & min(ifs_df$year) < cutoff) { 
    lh <- min(ifs_df$value) + label_height*(max(ifs_df$value) - min(ifs_df$value))
    p <- p + hf_line(lh,split=label_split,text_orient=text_orient)
  }
  if (dots) { p <- p + geom_point(size=2)}
  if (!is.null(ontop)) {
    p <- p + geom_line(data=ifs_df[ifs_df$Scenario==ontop,],size=1)
    if (dots) { p <- p + geom_point(data=ifs_df[ifs_df$Scenario==ontop,],size=2)}
  }
  p
}

## TODO: what other standardized plot types would it be good to have?

# comparison of multiple countries across multiple scenarios, encoding 
# countries with colors and scenarios with line-types (or vice versa)
# (example from report?)

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



