library(dplyr)


# load
df <- tuesdata$frogs

df$Female = as.factor(df$Female)

levels(df$Female) = list(Male = '0',
                         Female = '1')

# plot
df %>% 
  group_by(Subsite, Water) %>% 
  summarise(total = n()) %>%
  mutate(Water = factor(Water), 
         Subsite = factor(Subsite, levels = rev(site_levs)), 
         Subsite = fct_recode(Subsite,
                              `North East Reservoir` = "NE Res",
                              `North Reservoir` = "N Res", 
                              `West Reservoir` = "W Res", 
                              `South East Pond` = "SE Pond")) %>% 
  ggplot(aes(x = Subsite, y = total, fill = Water)) +
  scale_fill_brewer(direction = -1) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = '',
    y = 'Number of frogs',
    title = "Frog location",
    caption = "Data : Oregon Spotted Frog (TidyTuesday) | Graphic: Clément Rieux") +
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        plot.margin = unit(c(0.5, 3, 0.5, 0.5), "cm"), 
        # plot.background = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"), 
        # panel.background = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"), 
        # legend.background = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"), 
        legend.key = element_rect(colour = "#F8F0C6", fill = "#F8F0C6"),
        axis.ticks = element_blank(), 
        panel.grid = element_blank())


ggsave(paste0(currt_direct, '/plot/', date_file, "/spotted_frog.png"), 
       width = 10, height = 10) 
