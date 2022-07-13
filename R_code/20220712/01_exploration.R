library(dplyr)
library(ggplot2)
require(maps)
require(viridis)

# load
tuesdata <- tidytuesdayR::tt_load(date)
flights <- tuesdata$flights

flights = flights %>% 
  mutate(dep = pmax(FLT_DEP_1, FLT_DEP_IFR_2, na.rm = T),
         arr = pmax(FLT_ARR_1, FLT_ARR_IFR_2, na.rm = T),
         tot = pmax(FLT_TOT_1, FLT_TOT_IFR_2, na.rm = T)) %>% 
  select(-c(FLT_DEP_1, FLT_DEP_IFR_2, FLT_ARR_1, FLT_ARR_IFR_2, FLT_TOT_1, FLT_TOT_IFR_2))


coord = read.table(paste0(currt_direct, '/data/', date_file, '/icao_coord.txt'), header = TRUE, sep = ",", dec = ".")

flights = flights %>%
  left_join(coord %>%
              select(icao,
                     latitude,
                     longitude) %>%
              rename(APT_ICAO = icao))

flights$latitude = as.numeric(flights$latitude)
flights$longitude = as.numeric(flights$longitude)


flights_plot = flights %>% filter(FLT_DATE == as.POSIXct('2022-05-31', tz = 'UTC'))#,
                                  # STATE_NAME == 'France',
                                  # !grepl("Paris",APT_NAME))
       
library("sf") 
library("rnaturalearth")
library("rnaturalearthdata")

# crs_use = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs_use = "+proj=robin"

sf_europe <- st_as_sf(rworldmap::getMap(resolution = "high")) %>% 
  st_transform(crs = crs_use) %>%
  # filter(REGION == "Europe") %>% 
  st_crop(xmin = -4000000, xmax = 4300000, ymin = 2500000, ymax = 9000000) %>%
  mutate(
    area = st_area(geometry),
    centroid = st_centroid(geometry)
  ) %>% 
  dplyr::select(SOVEREIGNT, GLOCAF, area, centroid)


d_points <- flights_plot %>% 
  filter(is.na(longitude) == F,
         is.na(latitude) == F) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = crs_use)


library(RColorBrewer)


nb_groupe <- length(unique(d_points$STATE_NAME))
mycolors <- rep(colorRampPalette(brewer.pal(8, "Paired"))(nb_groupe))

match_col <- data.frame(unique(d_points$STATE_NAME), mycolors)
match_col <- match_col %>% rename(STATE_NAME = unique.d_points.STATE_NAME.)

d_points = d_points %>% 
  left_join(match_col)

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
                 alpha = tot,
                 size = tot)) +
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
