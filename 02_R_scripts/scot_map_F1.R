# data viz of low resolution rnaturalearth map of Scotland
# imagined as F1 circuit
# various designs as well known Scottish brands

# 1 Libraries----

library(dplyr)
library(readr)
library(ggplot2)
library(showtext)
library(ggtext)
library(sf)
library(rnaturalearth)
library(patchwork)
library(ggpubr)
library(jpeg)
library(maps)
library(DescTools)

# 2 Set fonts----
font_add_google("Luckiest Guy","ramp")
font_add_google("Bebas Neue","beb")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto", "roboto")
font_add_google("Anton", "anton")
font_add_google("Ultra", "ultra")
font_add_google("Abril Fatface", "abril")
font_add_google("Luckiest Guy", "lucky")
font_add_google("Rye", "rye")
showtext_auto()
showtext_opts(dpi = 320)

# 3 rnaturalearth map of scotland----
# https://luisdva.github.io/rstats/mapssf-eng/

scotland <- ne_countries(geounit = "scotland", type = "map_units", returnclass = "sf", scale = "small")
# st_centroid(scotland)
# centroid -4.1 lon, 56.6 lat

#scotland

# 3.1* Irn Bru----
# define point for Ayr Town Centre ('Ayrton Senna')
coordinates_ayr <- data.frame(
  place = 'AYR TOON',
  longitude = -4.629,
  latitude = 55.458
)

# project the coordinates
coordinates_sf_ayr <- coordinates_ayr |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

# define the google font
font <- 'anton'

# plot
plot_bru <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = '#DCD8EA', fill = '#0439E3') +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 6, color = "#0439E3", margin = margin(t = -25)),
        panel.background = element_rect(fill = "#FE851E"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = coordinates_sf_ayr, size = 4, pch=21, stroke = 2, fill = 'grey30') +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  annotate("text", y = 59.5, x = -4, label = "SCOTLAND", lineheight = 0.75, family = font,  size = 7, color = "#0439E3", vjust = 0.5) +
  annotate("text", y = 59.1, x = -4, label = "AS A F1 GRAND PRIX CIRCUIT", lineheight = 0.75, family = font,  size = 5, color = "#0439E3", vjust = 0.5) +
  annotate("text", y = 55.75, x = -4.4, label = "AYR", lineheight = 0.75, family = font,  size = 5, color = "#FE851E", hjust = 0) +
  annotate("text", y = 55.45, x = -4.25, label = "TOON", lineheight = 0.75, family = font,  size = 5, color = "#FE851E", hjust = 0) +
  annotate("text", y = 55.15, x = -4.75, label = "CENNA", lineheight = 0.75, family = font,  size = 5, color = "#FE851E", hjust = 0) +
  labs(caption = "Design: Allan James | @allanjames1506")

#plot_bru

# save plot
ggsave('./03_plots/scotmap_irn_bru.png', dpi = 320, height = 12, width = 9, units = 'cm')

# 3.2* Tunnocks----

# Import the background image
img <- readJPEG('./00_raw_data/tunnocks_t_cake_cropped3.jpg')

# define some chocolately/mallow colours
mallow <- '#f5ebec'
chocolate <- '#7B3F00'
dark_chocolate <- '#352728'

# define points for Isle of Lewis and Hamilton
coordinates_lewis_hamilton <- data.frame(
  place = c('LEWIS', 'HAMILTON'),
  longitude = c(-6.6616, -4.0323),
  latitude = c(58.2416, 55.7754)
)

# project the coordinates
coordinates_sf_lewis_hamilton <- coordinates_lewis_hamilton |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

# define some plot annotations
annotation1 <- data.frame(
  x = -4,
  y = 59.35,
  label = "Scotland"
)

annotation2 <- data.frame(
  x = -4,
  y = 54.25,
  label = "as a F1 Grand Prix circuit"
)

annotation3 <- data.frame(
  x = -4,
  y = 53.9,
  label = "design: Allan James | @allanjames1506"
)

# plot
plot_tunnocks <- ggplot() +
  background_image(img) +
  geom_sf(data=scotland, lwd = 4, colour = chocolate, fill = mallow) +
  theme(panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = coordinates_sf_lewis_hamilton, size = 10, pch=21, stroke = 3, fill = '#EE1A2E', colour = '#FFE404') +
  geom_sf_label(data = coordinates_sf_lewis_hamilton, aes(label = place), nudge_x = 0.3, nudge_y = 0.55, family = 'ultra', colour = '#EE1A2E', fill = '#FFE404', size = 5, label.size = 0.5) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  geom_label(data = annotation1, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#AB2733", 
             size = 6 , family = "ultra", hjust = 0.5) +
  geom_label(data = annotation2, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#AB2733", 
             size = 4 , family = "ultra", hjust = 0.5) +
  geom_label(data = annotation3, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = dark_chocolate, 
             size = 1.5 , family = "ultra", hjust = 0.5)

plot_tunnocks

# save plot
ggsave('./03_plots/scotmap_tunnocks.png', dpi = 320, height = 12, width = 9, units = 'cm')

# 3.3* Tennents----

# define fictional points for Tennents logo/ turning points
coordinates_tennents <- data.frame(
  place = c('T','T', 'T', 'T', 'T'),
  longitude = c(-3.7, -2.7196, -2.0076, -3.0689, -5),
  latitude = c(54.65, 56.0584, 57.6934, 58.6373, 58.65)
)

# define location of Turnberry in Ayrshire
coordinates_turnberry <- data.frame(
  place = 'TURN-BERRY',
  longitude = -4.8350,
  latitude = 55.3113
)

# project coordinates
coordinates_sf_tennents <- coordinates_tennents |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

coordinates_sf_turnberry <- coordinates_turnberry |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

# plot
plot_tennents <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = 'white', fill = '#B82B35') +
  theme(plot.caption = element_text(family = 'ultra', hjust = 0.5, size = 7, color = "#0439E3", margin = margin(t = -25)),
        panel.background = element_rect(fill = "#ECD747"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = coordinates_sf_tennents, size = 4, pch=21, stroke = 2, fill = '#ECD747', colour = '#F6A11D') +
  geom_sf(data = coordinates_sf_turnberry, size = 4, pch=21, stroke = 2, fill = '#ECD747', colour = '#F6A11D') +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = 'white', family = 'ultra', size = 10) +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = '#ECD747', family = 'ultra', size = 9) +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = 'black', family = 'ultra', size = 8) +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = '#B82B35', family = 'ultra', size = 7) +
  geom_sf_label(data = coordinates_sf_turnberry, aes(label = place), nudge_x = 0.6, nudge_y = 0.35, family = 'ultra', colour = 'black', size = 6, label.size = 0.5) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  labs(caption = "Design: Allan James | @allanjames1506") +
  annotate("text", y = 59.5, x = -4, label = "SCOTLAND", lineheight = 0.75, family = 'ultra',  size = 7, color = "#0439E3", vjust = 0.5) +
  annotate("text", y = 59.25, x = -4, label = "AS A F1 GRAND PRIX CIRCUIT", lineheight = 0.75, family = 'ultra',  size = 3, color = "#0439E3", vjust = 0.5) 

plot_tennents

# save plot
ggsave('./03_plots/scotmap_tennents.png', dpi = 320, height = 12, width = 9, units = 'cm')

# 3.4* Land O'Norris ----

# define some plot annotations
annotation1_lando <- data.frame(
  x = -4,
  y = 59.35,
  label = "Land O'Norris"
)

annotation2_lando <- data.frame(
  x = -4,
  y = 54.2,
  label = "design: Allan James | @allanjames1506"
)

# plot
plot_lando <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = '#FF8000', fill = '#DBFB51') +
  theme(panel.background = element_rect(fill = "#47c7fc"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  annotate("text", y = 57, x = -4.3, label = "LN", lineheight = 0.75, family = 'anton',  size = 14, color = "#47c7fc", hjust = 0.2, alpha = 0.9) +
  annotate("text", y = 56.4, x = -4.3, label = "04", lineheight = 0.75, family = 'anton',  size = 14, color = "#FF8000", hjust = 0.5, alpha = 0.5) +
  geom_label(data = annotation1_lando, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#000000",
             size = 8 , family = "rye", hjust = 0.5) +
  geom_label(data = annotation2_lando, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#000000", 
             size = 2.5 , family = "anton", hjust = 0.5)

plot_lando

# save plot
ggsave('./03_plots/scotmap_lando.png', dpi = 320, height = 12, width = 9, units = 'cm')

# 3.5* Glenfiddich----
# work in progress, possibly not used
# plan to make map in Glenfiddich style and strpes across Scotland in whisky colours
# learn from Ryand Hart curatedmess code - see section 4
# low resolution map results in low number of grid stripes
# did not turn out as intended
# keeping code to re-use if needed

# get the bounding box of points ------------------------------------------
# Calculate the range of longitude and latitude

#get the bounding box numbers for lat long
# scotland

# Bounding box:  xmin: -6.149981 ymin: 54.60094 xmax: -1.959281 ymax: 58.635

min_lon_scot <- -6.149981
max_lon_scot <- -1.959281
min_lat_scot <- 54.60094
max_lat_scot <- 58.635

# calculate the grid of points --------------------------------------------
df_points_scot <- expand_grid(x = seq(from = min_lon_scot, to = max_lon_scot, length.out = 10),
                              y = seq(from = min_lat_scot, to = max_lat_scot, length.out = 10))

# get the outline of points of map ----------------------------------------
glimpse(scotland$geometry)

scotland_unlist <- unlist(as_tibble(scotland))[c(TRUE,TRUE)]
scotland_unlist

scotland_unlist_lon <- scotland_unlist[64:87]

scotland_unlist_lat <- scotland_unlist[88:111]

st_centroid(scotland)
# centroid -4.1 lon, 56.6 lat

scotland_unlist_lon_tibble <- as_tibble(scotland_unlist_lon) %>% 
  rename(x = value)

scotland_unlist_lon_tibble$x_num = as.numeric(as.character(scotland_unlist_lon_tibble$x)) 

class(scotland_unlist_lon_tibble$x_num)

scotland_unlist_lon_tibble <- scotland_unlist_lon_tibble %>% 
  mutate(x_num_edit = case_when(x_num < -4.1 ~ x_num + 0.3,
                                x_num > -4.1 ~ x_num - 0.3)) 
#%>% 
dplyr::select(3) %>% 
  rename(x = x_num_edit)

scotland_unlist_lat_tibble <- as_tibble(scotland_unlist_lat) %>% 
  rename(y = value)

scotland_unlist_lat_tibble$y_num = as.numeric(as.character(scotland_unlist_lat_tibble$y)) 

scotland_unlist_lat_tibble <- scotland_unlist_lat_tibble %>% 
  dplyr::select(2) %>% 
  rename(y = y_num)

scotland_unlist_lat_lon_tibble <- scotland_unlist_lon_tibble %>% 
  cbind(scotland_unlist_lat_tibble)

# calculate the points from the grid within the bounding box line ---------

# seed
set.seed(777)

# kente colors
colors <- c("#00aff0", "#000000", "#fec000", "#00af50", "#ffff00", "#a6a6a6", "#f15be9", "#fafbfc", "#6c3b2b", "#ff0000", "#70309f", "#c0c0c0")

# create data frame for plotting ------------------------------------------
map_df <- data.frame(PtInPoly(df_points_scot, scotland_unlist_lat_lon_tibble)) %>% 
  filter(pip == 1) %>% 
  group_by(y) %>% 
  mutate(id = cur_group_id()) %>%
  mutate(color = sample(colors, 1, replace = TRUE)) %>%
  ungroup()

ggplot() +
  geom_path(data = map_df, aes(x, y, group = id, color = color), linewidth = 5, inherit.aes = FALSE) +
  geom_sf(data=scotland, lwd = 4, colour = '#A02423', fill = 'transparent') +
  scale_color_identity() +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  #annotate("text", y = 11, x = -4.35, label = "G\nh\na\nn\na", lineheight = 0.75, family = font, fontface = "bold", size = 14, color = "#000000", vjust = "top") +
  #annotate("text", y = 7.4, x = -4.6, label = "Kente\ncloth\ncolors", lineheight = 0.9, family = font, size = 6, color = "#000000", vjust = "top", hjust = "left") +
  #scale_x_continuous(limits = c(-6.5, NA)) +
  #coord_map() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#f7f7f5"),
        plot.background = element_rect(color = NA, fill = "#f7f7f5")) +
  labs(caption = "#30DayMapChallenge | Day 8: Africa | Design: Ryan Hart")


annotation1_lando <- data.frame(
  x = -4,
  y = 59.35,
  label = "Land O'Norris"
)

annotation2_lando <- data.frame(
  x = -4,
  y = 54.2,
  label = "design: Allan James | @allanjames1506"
)

lando <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = '#FF8000', fill = '#DBFB51') +
  theme(panel.background = element_rect(fill = "#47c7fc"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  annotate("text", y = 57, x = -4.3, label = "LN", lineheight = 0.75, family = 'anton',  size = 14, color = "#47c7fc", hjust = 0.2, alpha = 0.9) +
  annotate("text", y = 56.4, x = -4.3, label = "04", lineheight = 0.75, family = 'anton',  size = 14, color = "#FF8000", hjust = 0.5, alpha = 0.5) +
  geom_label(data = annotation1_lando, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#000000",
             size = 8 , family = "rye", hjust = 0.5) +
  geom_label(data = annotation2_lando, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#000000", 
             size = 2.5 , family = "anton", hjust = 0.5)

lando

ggsave('./03_plots/lando_bday.png', dpi = 320, height = 12, width = 9, units = 'cm')

#4 Ryan Hart curratedmess Kente map to learn from ----

# 30DayMapChallenge | November 8, 2023 | Africa

# libraries ---------------------------------------------------------------
# library(tidyverse)
# library(showtext)
# library(maps)
# library(sf)
# library(DescTools)

# add font ----------------------------------------------------------------
font_add_google(name = "Besley", family = "Besley")
font <- "Besley"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get map boundary data ---------------------------------------------------
ghana <- map_data('world')[map_data('world')$region == "Ghana",] 

# get the bounding box of points ------------------------------------------
# Calculate the range of longitude and latitude
min_lon <- min(ghana$long)
max_lon <- max(ghana$long)
min_lat <- min(ghana$lat)
max_lat <- max(ghana$lat)

# calculate the grid of points --------------------------------------------
df_points <- expand_grid(x = seq(from = min_lon, to = max_lon, length.out = 100),
                         y = seq(from = min_lat, to = max_lat, length.out = 100))

# get the outline of points of map ----------------------------------------
ghana_map_line <- ghana %>% 
  dplyr::select(1:2) %>% 
  rename(x = long, y = lat)

# calculate the points from the grid within the bounding box line ---------

# seed
set.seed(777)

# kente colors
colors <- c("#00aff0", "#000000", "#fec000", "#00af50", "#ffff00", "#a6a6a6", "#f15be9", "#fafbfc", "#6c3b2b", "#ff0000", "#70309f", "#c0c0c0")

# create data frame for plotting ------------------------------------------
map_df <- data.frame(PtInPoly(df_points, ghana_map_line)) %>% 
  filter(pip == 1) %>% 
  group_by(y) %>% 
  mutate(id = cur_group_id()) %>%
  mutate(color = sample(colors, 1, replace = TRUE)) %>%
  ungroup()

# create plot -------------------------------------------------------------
ggplot() +
  geom_path(data = map_df, aes(x, y, group = id, color = color), linewidth = 1.3) +
  scale_color_identity() +
  annotate("text", y = 11, x = -4.35, label = "G\nh\na\nn\na", lineheight = 0.75, family = font, fontface = "bold", size = 14, color = "#000000", vjust = "top") +
  annotate("text", y = 7.4, x = -4.6, label = "Kente\ncloth\ncolors", lineheight = 0.9, family = font, size = 6, color = "#000000", vjust = "top", hjust = "left") +
  scale_x_continuous(limits = c(-4.7, NA)) +
  coord_map() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#f7f7f5"),
        plot.background = element_rect(color = NA, fill = "#f7f7f5")) +
  labs(caption = "#30DayMapChallenge | Day 8: Africa | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("ghana_kente_colors_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

