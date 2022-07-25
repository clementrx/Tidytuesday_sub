library(dplyr)
library(ggplot2)
require(maps)
require(viridis)
library(sf) 
library(RColorBrewer)
library(tidyverse)
library(ggtext)

# load
tuesdata <- tidytuesdayR::tt_load(date)
df <- tuesdata$technology

# clean it
df_plot = df %>% 
  filter(group == 'Production',
         # iso3c %in% c('ALB',
         #              'DEU',
         #              'AND',
         #              'AUT',
         #              'BEL',
         #              'BLR',
         #              'BIH',
         #              'BGR',
         #              'CYP',
         #              'HRV',
         #              'DNK',
         #              'ESP',
         #              'EST',
         #              'FIN',
         #              'FRA',
         #              'GRC',
         #              'HUN',
         #              'IRL',
         #              'ISL',
         #              'ITA',
         #              'KA',
         #              'LIE',
         #              'LTU'
         # ),
         iso3c %in% c('FRA', 'DEU', 'ESP', 'ITA', 'GBR', 'BEL'),
         category == 'Energy',
         grepl('from', label)) %>% 
  mutate(label = gsub('(TWH)', '', label),
         label = gsub("[()]", '', label),
         iso3c = case_when(iso3c == "FRA" ~ "France",
                           iso3c == "DEU" ~ "Allemagne",
                           iso3c == "ESP" ~ "Espagne",
                           iso3c == "ITA" ~ "Italie",
                           iso3c == "GBR" ~ "Angleterre",
                           iso3c == "BEL" ~ "Belgique",
                           TRUE ~ as.character(iso3c)))

df_plot_perc = df_plot %>% 
  group_by(year, label, iso3c) %>% 
  summarise(Total = sum(value)) %>% 
  ungroup() %>% 
  group_by(year, iso3c) %>% 
  mutate(Total_y = sum(Total),
         perc = Total/Total_y) %>% 
  ungroup() %>% 
  group_by(label, iso3c) %>% 
  mutate(
    end = max(Total)) %>% 
    ungroup() #%>% 
    # mutate(label.x = fct_reorder(label, end),
           # label_rev = fct_rev(label.x))


ggplot(df_plot_perc,
       aes(x = year, y = Total, color = label, fill = label,
           group = label)) +
  # geom_point() +
  # geom_line() + 
  geom_area(position = "stack",size = 0)+
  scale_fill_viridis(discrete=TRUE, option = "D") +
  scale_y_continuous(name = "Electricity (TWH)", 
                     breaks = seq(from = 0, to = 600, by = 100)) +  
  scale_x_continuous(name = '', breaks = seq(from = 1985, to = 2020, by = 5)) + 
  labs(
    title = "Source from electricity",
    caption = "Data : Technology (TidyTuesday)\nGraphic: Clément Rieux") +
  mytheme +
  facet_wrap(.~iso3c)
  

# theme
mytheme <- theme(text = element_text(family = 'Avenir')
                 # ,panel.grid.major = element_line(color = '#cccccc'
                 #                                  ,linetype = 'dashed'
                 #                                  ,size = .3
                 # )
                 ,panel.grid = element_line(colour = "transparent")
                 ,panel.background = element_rect(fill = 'lightblue')
                 # ,plot.title = element_text(size = 32)
                 # ,plot.subtitle = element_text(size = 14)
                 # axis.title = element_blank()
                 # axis.text = element_blank()
                 # axis.ticks = element_blank()
                 ,axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
                 ,plot.title = element_text(hjust = 0.1)
                 ,plot.caption.position = "plot"
                 ,plot.caption = element_text(vjust = 5, size = 8, hjust = 0.5)
                 # ,legend.position = 'none'
                 # ,legend.text = element_text(size=6)
                 # ,legend.key.size = unit(0.5, 'cm')
)

ggsave(paste0(currt_direct, '/plot/', date_file, "/airport_traffic_map.png"), 
       width = 10, height = 8) 
