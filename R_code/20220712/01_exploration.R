library(dplyr)
library(ggplot2)
require(maps)
require(viridis)
library(sf) 
library(RColorBrewer)

# load
tuesdata <- tidytuesdayR::tt_load(date)
flights <- tuesdata$flights

# clean it
flights = flights %>% 
  mutate(dep = pmax(FLT_DEP_1, FLT_DEP_IFR_2, na.rm = T),
         arr = pmax(FLT_ARR_1, FLT_ARR_IFR_2, na.rm = T),
         tot = pmax(FLT_TOT_1, FLT_TOT_IFR_2, na.rm = T)) %>% 
  select(-c(FLT_DEP_1, FLT_DEP_IFR_2, FLT_ARR_1, FLT_ARR_IFR_2, FLT_TOT_1, FLT_TOT_IFR_2))

# get long/lat from ICAO code
coord = read.table(paste0(currt_direct, '/data/', date_file, '/icao_coord.txt'), header = TRUE, sep = ",", dec = ".")

flights = flights %>%
  left_join(coord %>%
              select(icao,
                     latitude,
                     longitude) %>%
              rename(APT_ICAO = icao))

flights$latitude = as.numeric(flights$latitude)
flights$longitude = as.numeric(flights$longitude)

# Number total IFR movements by airport in 2021
flights_plot_2021 = flights %>% 
  filter(YEAR == "2021") %>% 
  group_by(APT_NAME, STATE_NAME, longitude, latitude) %>% 
  summarise(Total = sum(tot)) %>% 
  ungroup()
       
# get Europe Map
crs_use = "+proj=robin"

sf_europe <- st_as_sf(rworldmap::getMap(resolution = "high")) %>% 
  st_transform(crs = crs_use) %>%
  # filter(REGION == "Europe") %>% 
  st_crop(xmin = -4000000, xmax = 4300000, ymin = 2500000, ymax = 7500000) %>%
  mutate(
    area = st_area(geometry),
    centroid = st_centroid(geometry)
  ) %>% 
  dplyr::select(SOVEREIGNT, GLOCAF, area, centroid)

# transform long / lat coord
d_points <- flights_plot_2021 %>% 
  filter(is.na(longitude) == F,
         is.na(latitude) == F) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = crs_use)

# generate color by country
nb_groupe <- length(unique(d_points$STATE_NAME))
mycolors <- rep(colorRampPalette(brewer.pal(8, "Paired"))(nb_groupe))

match_col <- data.frame(unique(d_points$STATE_NAME), mycolors)
match_col <- match_col %>% rename(STATE_NAME = unique.d_points.STATE_NAME.)
d_points = d_points %>% 
  left_join(match_col)

# theme
mytheme <- theme(text = element_text(family = 'Avenir')
                 # ,panel.grid.major = element_line(color = '#cccccc' 
                 #                                  ,linetype = 'dashed'
                 #                                  ,size = .3
                 # )
                 ,panel.grid.major = element_line(colour = "transparent")
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 32)
                 ,plot.subtitle = element_text(size = 14)
                 ,axis.title = element_blank()
                 ,axis.text = element_blank()
                 ,axis.ticks = element_blank()
)

map <- ggplot(sf_europe) +
  rcartocolor::scale_fill_carto_c(palette = "BluYl") +
  geom_sf(color = "grey85",
          fill = "grey90",
          lwd = 0.1) +
  geom_sf(data= d_points,
             aes(color = mycolors,
                 alpha = Total,
                 size = Total)) +
  # scale_color_viridis() +
  # scale_x_continuous(expand = c(0, 0),
  #                    limits = c(-3300000, NA)) +
  # scale_y_continuous(expand = c(0.02, 0.02)) +
  mytheme + 
  # theme_void() + 
  theme(plot.margin = margin(0, 0, 0, 0)) +
  guides(color="none",
         alpha = 'none', 
         size = 'none')

map

ggsave(paste0(currt_direct, '/plot/', date_file, "/first_test.png"), 
       width = 5, height = 5) 


# ===

library(gganimate)


top5_airport_cntrt = flights_plot_2021 %>%
  filter(STATE_NAME %in% c("Spain", "France", "United Kingdom", "Italy", "Germany")) %>% 
  arrange(desc(Total)) %>% 
  group_by(STATE_NAME) %>% 
  slice(1:5) %>% 
  select(-c(longitude, latitude))

top5_airport_cntrt = top5_airport_cntrt %>% 
  left_join(match_col)

top5_airport_cntrt$id <- seq(1, nrow(top5_airport_cntrt))

# Get the name and the y position of each label
label_data <- top5_airport_cntrt
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- top5_airport_cntrt %>% 
  group_by(STATE_NAME) %>% 
  summarize(start=min(id), end=max(id) - 4) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(top5_airport_cntrt, aes(x=as.factor(id), y=Total)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", 
           aes(fill= mycolors,
               alpha = Total))  +
  coord_polar() +
  geom_bar(aes(x=as.factor(id), y=Total, fill=mycolors), stat="identity", alpha=0.5) +
  ylim(-300000,400000) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  geom_text(data=label_data, aes(x=id, y=Total+10, label=APT_NAME, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=STATE_NAME), 
            hjust=c(-1,1.2,3,1,-0.1),
            vjust=c(6,7,0,-7,-1), 
            colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p


#### animate

plot_filter = flights %>% 
  filter(APT_NAME %in% unique(top5_airport_cntrt$APT_NAME)) %>% 
  group_by(APT_NAME, STATE_NAME, YEAR) %>% 
  summarise(Total = sum(tot)) %>% 
  ungroup() %>% 
  arrange(desc(Total)) 

# WP = ggplot(data = plot_filter) +
#   geom_col(mapping = aes(x = APT_NAME, y = Total), 
#            fill = "darkcyan") +
#   theme_classic() +
#   xlab("Région") +
#   ylab("Nombre de téléphones (en milliers)") +
#   transition_time(YEAR) +
#   labs(title = "Year: {frame_time}") 
# 
# WP


top5_airport_cntrt$id <- seq(1, nrow(top5_airport_cntrt))

id = top5_airport_cntrt %>% select(APT_NAME, id)

plot_filter = plot_filter %>% 
  left_join(match_col)

plot_filter = plot_filter %>% 
  left_join(id)

plot_filter$YEAR = as.integer(plot_filter$YEAR)

# Get the name and the y position of each label
label_data <- plot_filter
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- plot_filter %>% 
  group_by(STATE_NAME) %>% 
  summarize(start=min(id), end=max(id) - 4) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(plot_filter, aes(x=as.factor(id), y=Total)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", 
           aes(fill= mycolors,
               alpha = Total))  +
  coord_polar() +
  geom_bar(aes(x=as.factor(id), y=Total, fill=mycolors), stat="identity", alpha=0.5) +
  ylim(-300000,400000) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  geom_text(data=label_data, aes(x=id, y=Total+10, label=APT_NAME, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=STATE_NAME), 
            hjust=c(-1,1.2,3,1,-0.1),
            vjust=c(6,7,0,-7,-1), 
            colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
  transition_time(YEAR) +
  labs(title = "Year: {frame_time}") 

p


ggplot(plot_filter, aes(x=as.factor(id), y=Total)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", 
           aes(fill= mycolors,
               alpha = Total))  +
  coord_polar() +
  transition_time(YEAR) +
  labs(title = "Year: {frame_time}") 
