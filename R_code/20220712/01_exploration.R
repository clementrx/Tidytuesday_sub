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

flights_plot = flights %>% filter(FLT_DATE == as.POSIXct('2022-05-31', tz = 'UTC'),
                                  STATE_NAME == 'France',
                                  !grepl("Paris",APT_NAME))
       
library("sf") 
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
eu = world %>% filter(continent == "Europe")

ggplot(data = world) +
  geom_sf() + 
  coord_sf(ylim = c(41.77, 53.83), xlim = c(-6.26, 9.97), expand = FALSE) +
  geom_point(data= flights_plot,
             aes(x=longitude, y=latitude, color = tot)) +
  scale_color_viridis()

ggsave(paste0(currt_direct, '/plot/', date_file, "/first_test.png"), 
       width = 10, height = 10) 



