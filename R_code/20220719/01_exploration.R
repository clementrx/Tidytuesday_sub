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
         iso3c %in% c('FRA', 'DEU', 'ESP', 'ITA', 'GBR', 'USA', 'RUS', 'CHN', 'SWE'),
         category == 'Energy',
         grepl('from', label)) %>% 
  mutate(label = gsub('(TWH)', '', label),
         label = gsub("[()]", '', label),
         iso3c = case_when(iso3c == "FRA" ~ "France",
                           iso3c == "DEU" ~ "Allemagne",
                           iso3c == "ESP" ~ "Espagne",
                           iso3c == "ITA" ~ "Italie",
                           iso3c == "GBR" ~ "Angleterre",
                           iso3c == "USA" ~ "USA",
                           iso3c == "RUS" ~ "Russia",
                           iso3c == "CHN" ~ "China",
                           iso3c == 'SWE' ~ "Sweden",
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
                 ,legend.text = element_text(size=6)
                 ,legend.title = element_blank()
                 ,legend.key.size = unit(0.5, 'cm')
                 ,strip.background = element_blank()
)


ggplot(df_plot_perc,
       aes(x = year, y = perc, color = label, fill = label,
           group = label)) +
  geom_area(position = "stack",size = 0)+
  scale_fill_viridis(discrete=TRUE, option = "G") +
  scale_y_continuous(name = "Electricity (TWH)", labels = scales::percent) + 
                     # breaks = seq(from = 0, to = 600, by = 100)) +  
  scale_x_continuous(name = '', breaks = seq(from = 1985, to = 2020, by = 5)) + 
  labs(
    title = "Where does the electricity come from ?",
    caption = "Data : Technology Adoption (TidyTuesday)\nGraphic: Clément Rieux") +
  mytheme +
  facet_wrap(.~iso3c)

# fr = df_plot_perc %>% filter(year == "2020",
#                         iso3c == 'France') %>% 
#   # Basic piechart
#   ggplot(aes(x="", y=perc, fill=label)) +
#   geom_bar(stat="identity", width=1, position = "stack") +
#   coord_polar("y", start=0) +
#   scale_fill_viridis(discrete=TRUE, option = "G") +
#   geom_text(aes(x = 1.6, 
#                 label = scales::percent(perc, accuracy = .1)), 
#             position = position_stack(vjust = .6),
#             size = 3) +
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank(),
#         legend.position="none",
#         panel.background = element_rect(fill = 'white')) 

  
ggsave(paste0(currt_direct, '/plot/', date_file, "/electricity_adoption.png"), 
       width = 10, height = 10) 
