
library(tidyverse)
library(here)
library(shades) # for dulling color
library(scales)

# Code to build the du Bois plot ------------------------------------------

# (the plot: https://www.smithsonianmag.com/history/first-time-together-and-color-book-displays-web-du-bois-visionary-infographics-180970826/)

# some parameters
seg_size <- 4
col_red <- "#de213c"
col_yellow <- "#f9b915"
col_blue <- "#5072b2"
col_green <- "#3e6454"

# Make red spiral --------------------------------------------------------------

# data for red spiral (#ecded1)
# (from SO: https://stackoverflow.com/questions/41391271/plot-an-archimedean-spiral-using-integer-values-with-ggplot2)

# adjusted the cosine coefficient to get a good number of spirals, and to have 
# it open up the right way

t <- seq(0, 10, by=0.01)
df <- tibble(x = t * cos(-2.915*t), 
             y = t * sin(-2.915*t))

spiral <- df %>% 
  ggplot(aes(x, y)) +
  geom_path(color = col_red, size = seg_size) +
  xlim(-29, 29) +
  ylim(-14, 34) +
  theme(panel.background = element_rect(fill = "#F7eee5", color = "#F7eee5"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggeasy::easy_remove_axes()


# Function that builds a du Bois plot segment -----------------------------

build_seg <- function(x_start, y_start, x_end, y_end, color_hex) {
  geom_segment(data = tibble(x = x_start, y = y_start, xend = x_end, yend = y_end),
               mapping = aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               color = color_hex,
               #alpha = .2,
               size = seg_size,
               lineend = "round")
}

seg_red <- build_seg(x_start = -6.8, y_start = 7.3, x_end = 10.8, y_end = 23.3,
                     color_hex = col_red)

seg_yellow <- build_seg(x_start = 10.8, y_start = 23.3, x_end = 0, y_end = 30,
                        color_hex = col_yellow)

seg_blue <- build_seg(x_start = 0, y_start = 30, x_end = 3, y_end = 33,
                      color_hex = col_blue)

seg_green <- build_seg(x_start = 3, y_start = 33, x_end = -15, y_end = 33,
                       color_hex = col_green)

# Build the segments, add onto plot, and save -----------------------------

spiral + 
  seg_red +
  seg_yellow + 
  seg_blue +
  seg_green

ggsave(here("plots", "01_rural-vs-city-for-resume.png"))


