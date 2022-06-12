# flow fields
# ideas and code from https://jiwanheo.rbind.io/post/2021-09-17-how-to-work-with-flow-fields-in-r/
# https://github.com/jiwanheo/RecreationThursday/blob/main/week07/code.Rmd

library(tidyverse)

# testing out the basics ----
# make grid with angles
my_grid <- crossing(
  x = 1:10,
  y = 1:10
) %>%
  mutate(angle = (y / 10) * 2*pi)

my_flow_field <- matrix(data = my_grid$angle, nrow = 10, ncol = 10)

# show in viz with segments
grid_viz <- my_grid %>%
  mutate(
    xend = x + cos(angle) * 0.5,
    yend = y + sin(angle) * 0.5
  ) %>%
  ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_point(aes(x = x, y = y)) +
  coord_equal()

grid_viz

# make a tibble of "particles" to drop
my_particles <- tibble(
  x = c(2.5, 8.5),
  y = c(2.5, 4.5)
)

grid_viz + 
  geom_point(data = my_particles, aes(x = x, y = y), 
             colour = "red", size = 3)

# iterate over steps using functions ----
my_grid <- crossing(
  x = 1:10,
  y = 1:10
) %>%
  mutate(angle = (y / 10) * 2*pi)

my_flow_field <- matrix(data = my_grid$angle, nrow = 10, ncol = 10)

my_particles <- tibble(
  x = c(2.5, 8.5),
  y = c(2.5, 4.5)
)

get_closest_angle <- function(x, y, flow_field) {
  max_x <- ncol(flow_field)
  max_y <- nrow(flow_field)
  
  closest_x_index <- which(abs(1:max_x - x) == min(abs(1:max_x - x)))[[1]]
  closest_y_index <- which(abs(1:max_y - y) == min(abs(1:max_y - y)))[[1]]
  closest_angle <- flow_field[[closest_y_index, closest_x_index]]
  
  return(closest_angle)
}

next_angle <- my_particles %>%
  pmap_dbl(get_closest_angle, flow_field = my_flow_field)

my_particles <- my_particles %>%
  mutate(angle = next_angle) %>%
  mutate(xend = x + cos(angle) * 2.8,
         yend = y + sin(angle) * 2.8)

grid_viz +
  geom_segment(data = my_particles, aes(x = x, y = y, xend = xend, yend = yend),
               colour = "red") +
  geom_point(data = my_particles, aes(x = x, y = y), colour = "red", size = 3)

source("flow-fields/functions/generate_flow_field.R")
test <- generate_flow_field()

test$visualised_flow_field %>% ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))
