library(tidyverse)
library(sp)
library(ggrepel)

brazil <- readRDS('gadm36_BRA_1_sp.rds')
brazil@data$id <- rownames(brazil@data)
brazil_points <- fortify(brazil, region = "id")
plotme <- left_join(brazil_points, brazil@data, by = "id")

left_join(plotme,dummy,by='NAME_1') %>%
ggplot(aes(x=long, y=lat, group = group,fill = value)) +
  geom_polygon()  +
  geom_path(color = "white") +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())

## get this to work with an IFs output series

rename_df <- tibble(NAME_1=c("Amapá","Ceará","Espírito Santo","Goiás",
                             "Maranhão","Pará","Paraíba","Paraná","Piauí",
                             "Rondônia","São Paulo"),
                    name=c("Amapa","Ceara","Espirito Santo","Goias",
                           "Maranhao","Para","Paraiba","Parana","Piaui",
                           "Rondonia","Sao Paulo"))

pov_310 <- read_tsv('IFs_exports/pov_310.txt',col_names=c('name','year','value')) %>%
  na.omit %>%
  mutate(name=sub('BRA-','',name)) %>%
  left_join(rename_df,by='name') %>%
  mutate(NAME_1=ifelse(is.na(NAME_1),name,NAME_1)) %>%
  filter(year==2019)

# TODO: I'm not 100% confident that this is the right way to define a centroid
centroids <- plotme %>%
  group_by(NAME_1) %>%
  summarize(lat = median(lat),long=median(long))

left_join(plotme,pov_310,by='NAME_1') %>%
  ggplot(aes(x=long, y=lat, group = group,fill = value)) +
  geom_polygon()  +
  geom_path(color = "white") +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank()) +
  geom_point(data=centroids,aes(x=long,y=lat,group=NULL,fill=NULL),color='#BA0C2F') +
  geom_text_repel(data=centroids,aes(x=long,y=lat,label=NAME_1,group=NULL,fill=NULL),color='#BA0C2F')

# TODO: package this into a function that takes year and filename as arguments,
# makes a nice-looking plot with color scale that makes sense

# TODO: how would I make a plot with circles showing extensive variables?