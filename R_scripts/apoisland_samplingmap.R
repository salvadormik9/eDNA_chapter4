#### Load Packages ####
library(geodata)
library(sf)
library(ggplot2)
library(tidyverse)
library(janitor)
library(readxl)
library(ggrepel)
library(ggspatial)

#### Make Polygon of Apo Island ####
phl <- gadm("PHL", 
            level = 3, 
            path = ".") %>%
  st_as_sf()

apo_island <- phl %>%
  st_make_valid() %>%
  st_crop(xmin = 123.25, 
          xmax = 123.30, 
          ymin = 9.05, 
          ymax = 9.10)

plot(st_geometry(apo_island)) 
# This is a check. Apo Island should show up!

#### Read In and Clean Sampling sites ####
apo <- read_excel("inputs/apoisland_samplingsites.xlsx") %>%
  clean_names() %>%
  separate_wider_delim(
    cols = all_coordinates,
    delim = ",",
    names = c("longitude", 
              "latitude", 
              "altitude")) %>%
  mutate(across(c(longitude, 
                  latitude, 
                  altitude),
                ~ as.numeric(trimws(.x))))

apo_sites <- st_as_sf(apo, 
                      coords = c("longitude",
                                 "latitude"), 
                      crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2])

# Extent: smaller pad = more zoomed in
bb   <- st_bbox(apo_sites)
pad  <- 0.003
xlim <- c(bb["xmin"] - pad, 
          bb["xmax"] + pad)
ylim <- c(bb["ymin"] - pad, 
          bb["ymax"] + pad)

#### Create Mapping Plot ####
apo_map <- ggplot() +
  geom_sf(data = apo_island,
          fill = "#F2F0DC", 
          colour = "grey25", 
          linewidth = 0.4) +
  geom_sf(data = apo_sites,
          colour = "#88CFA4", 
          size = 5) +
  geom_label_repel(
    data = st_drop_geometry(apo_sites),
    aes(x = lon, 
        y = lat, 
        label = sampling_site),
    min.segment.length = 0, 
    size = 7,
    box.padding = .5,
    force = 3,               
    force_pull = 3,
    max.overlaps = Inf, 
    seed = 1
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(xlim = xlim, 
           ylim = ylim, 
           expand = FALSE,
           datum = st_crs(4326)) +                 # degree labels on the axes
  scale_x_continuous(breaks = seq(123.25, 
                                  123.30, 
                                  by = 0.005)) +
  scale_y_continuous(breaks = seq(9.05, 
                                  9.10, 
                                  by = 0.005)) +
  labs(x = "Longitude", 
       y = "Latitude") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#BDF0F0"),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(colour = "black", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(colour = "black", size = 9)
  )

apo_map


ggsave("outputs/apo_map.png",
       device = "png",
       units = "in",
       dpi = 300,
       width = 8,
       height = 10
       )
