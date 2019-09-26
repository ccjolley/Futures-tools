library(tidyverse)
library(sp)
library(ggrepel)

source('../Futures-tools/IFs_plots.R')

brazil_init <- function() {
  brazil <- readRDS('gadm36_BRA_1_sp.rds')
  brazil@data$id <- rownames(brazil@data)
  brazil_points <- fortify(brazil, region = "id")
  brazil_plot <<- left_join(brazil_points, brazil@data, by = "id")
  centroids <<- brazil_plot %>%
    filter(!(NAME_1=='Santa Catarina' & lat > -20)) %>%
    filter(!(NAME_1=="Paraíba" & long < -40)) %>%
    filter(!(NAME_1=="Rio Grande do Norte" & long > -34)) %>%
    filter(!(NAME_1=="Espírito Santo" & long > -36)) %>%
    group_by(NAME_1) %>%
    summarize(lat = 0.5*(min(lat)+max(lat)),
              long = 0.5*(min(long)+max(long)))
  rename_df <<- tibble(NAME_1=c("Amapá","Ceará","Espírito Santo","Goiás",
                               "Maranhão","Pará","Paraíba","Paraná","Piauí",
                               "Rondônia","São Paulo"),
                      name=c("Amapa","Ceara","Espirito Santo","Goias",
                             "Maranhao","Para","Paraiba","Parana","Piaui",
                             "Rondonia","Sao Paulo"))
}

ifs_choropleth <- function(input,display_year=2019,low_white=TRUE,percent=FALSE,percent_acc=1,minmax=c(NA,NA)) {
  if (typeof(input) == 'character') {
    input <- read_tsv(input,col_names=c('name','year','value'))
  } 
  ifs <- input %>%  
    na.omit %>%
    mutate(name=sub('BRA-','',name)) %>%
    left_join(rename_df,by='name') %>%
    mutate(NAME_1=ifelse(is.na(NAME_1),name,NAME_1)) %>%
    filter(year==display_year)
  if (percent) {
    ifs <- mutate(ifs,value=value/100)
    my_labels <- scales::percent_format(accuracy=percent_acc)
  } else {
    my_labels <- waiver()
  }
  if (low_white) {
   low_color <- 'white'
   high_color <- '#0067B9'
  } else {
   low_color <- '#0067B9'
   high_color <- 'white'
  }
  left_join(brazil_plot,ifs,by='NAME_1') %>%
    ggplot(aes(x=long, y=lat, group = group,fill = value)) +
    geom_polygon()  +
    geom_path(color = '#6C6463',size=0.2) +
    coord_equal() +
    theme_USAID + 
    scale_fill_gradient(low=low_color,high=high_color, labels=my_labels, limits=minmax) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.title=element_blank()) 
}

adm1_labels <- list(
  geom_point(data=centroids,aes(x=long,y=lat,group=NULL,fill=NULL),color='#BA0C2F'),
  geom_text_repel(data=centroids,aes(x=long,y=lat,label=NAME_1,group=NULL,fill=NULL),color='#BA0C2F')
)

brazil_init()
ifs_choropleth('IFs_exports/pov_310.txt',display_year=2005,percent=TRUE,minmax=c(0,0.55)) +
  ggtitle('Poverty rate (< $3.10/day), 2005')

ifs_choropleth('IFs_exports/pov_310.txt',display_year=2019,percent=TRUE,minmax=c(0,0.55)) +
  ggtitle('Poverty rate (< $3.10/day), 2019')

ifs_choropleth('IFs_exports/pov_310.txt',display_year=2035,percent=TRUE,minmax=c(0,0.55)) +
  ggtitle('Poverty rate (< $3.10/day), 2035')

# TODO: make a plot with circles showing extensive variables?

fname <- 'IFs_exports/gdp_mer.txt'
display_year <- 2019

ifs_circles <- function(fname,display_year,maxsize=6,minmax=c(NA,NA)) {
  ifs <- read_tsv(fname,col_names=c('name','year','value')) %>%
    na.omit %>%
    mutate(name=sub('BRA-','',name)) %>%
    left_join(rename_df,by='name') %>%
    mutate(NAME_1=ifelse(is.na(NAME_1),name,NAME_1)) %>%
    filter(year==display_year)
  
  add_pts <- centroids %>%
    left_join(ifs,by='NAME_1') %>%
    select(lat,long,NAME_1,value)
  
  ggplot(brazil_plot,aes(x=long, y=lat, group = group)) +
    geom_polygon(fill='white')  +
    geom_path(color = '#6C6463',size=0.2) +
    coord_equal() +
    theme_USAID + 
    theme(axis.text = element_blank(),
          axis.title = element_blank()) +
    geom_point(data=add_pts,aes(x=long,y=lat,size=value, group=NULL),
               color='#BA0C2F') +
    scale_size_area(max_size=maxsize,limits=minmax) 
}

# Not actually such an interesting example, but the plots work

ifs_circles('IFs_exports/gdp_mer.txt',2019,maxsize=10,minmax=c(5,2000)) +
  labs(title='GDP (MER) in 2019',size='Billion USD')

ifs_circles('IFs_exports/gdp_mer.txt',1995,maxsize=10,minmax=c(5,2000)) +
  labs(title='GDP (MER) in 1995',size='Billion USD')

ifs_circles('IFs_exports/gdp_mer.txt',2050,maxsize=10,minmax=c(5,2000)) +
  labs(title='GDP (MER) in 2050',size='Billion USD')

### Interesting visualizations I could try (show each together with line plots)

###############################################################################
# Growth rate
###############################################################################
growth_choropleth <- function(fname,center_year=2019,window=5,type='percent') {
  growth_rate <- function(d,type) {
    lm1 <- lm(value ~ year,data=d)
    if (type=='percent') {
      rate <- 100*lm1$coefficients[2] / mean(d$value) 
    } else {
      rate <- lm1$coefficients[2]
    }
    tibble(value=rate)
  }
  gdp_growth <- read_tsv(fname,col_names=c('name','year','value')) %>%
    na.omit %>%
    filter(year >= center_year-(window/2),year <= center_year+(window/2)) %>% 
    group_by(name) %>%
    group_modify(~ growth_rate(.x,type)) %>%
    ungroup %>%
    mutate(year=center_year) 
  ifs_choropleth(gdp_growth,display_year=center_year,percent=(type=='percent'),
                 percent_acc=0.1)
}

growth_choropleth('IFs_exports/gdp_mer.txt',2019) + 
  ggtitle('GDP growth rate, 2019')

growth_choropleth('IFs_exports/gdp_mer.txt',2035) + 
  ggtitle('GDP growth rate, 2035')

growth_choropleth('IFs_exports/gdp_mer.txt',2019,type='slope') +
  ggtitle('GDP growth, billion USD per year (2019)')

###############################################################################
# Year at which a certain threshold is crossed 
###############################################################################
threshold <- 70
fname <- 'IFs_exports/sanitation.txt'
start_yr <- 2015
end_yr <- 2100

get_crossing <- function(d,thresh,increase=TRUE) {
  # flip sign if we're looking for a decrease instead of an increase
  if (!increase) {
    d$value <- -1 * d$value
  }
  if (min(d$value) > thresh) { return(tibble(value=-Inf)) } # crosses before time window
  if (max(d$value) < thresh) { return(tibble(value=Inf)) } # crosses after time window
  d <- arrange(d,year)
  tmp <- rbind(filter(d,value<=thresh) %>% tail(1),
               filter(d,value>=thresh) %>% head(1))
  tibble(value=tmp$year[1] + (tmp$year[2] - tmp$year[1])*(thresh-tmp$value[1])/(tmp$value[2] - tmp$value[1]))
}

ifs <- read_tsv(fname,col_names=c('name','year','value')) %>%
  na.omit %>%
  filter(year >= start_yr,year <= end_yr) %>%
  mutate(name=sub(', .*','',name))
         
start_yr <- min(ifs$year)
end_yr <- max(ifs$year)

ifs %>% 
  group_by(name) %>%
  group_modify(~ get_crossing(.x,threshold,TRUE))
# TODO: make sure these look right


# Year of peak value (demographic dividend)

# Animated choropleth (rural electricity access)

# BIG TODO: unpack Brazil scenarios