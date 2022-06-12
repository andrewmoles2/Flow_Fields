# generate flow field function

library(ambient)

generate_flow_field <- function(flow_field_width = 1000,
                                resolution_factor = 0.025,
                                perlin_scale_factor = 0.005,
                                perlin_seed = 1,
                                perlin_freq = 0.025) {
  
  resolution <- flow_field_width * resolution_factor
  num_cols <- flow_field_width %/% resolution
  num_rows <- num_cols
  
  long_grid_ff <- ambient::long_grid(x = 1:num_cols,
                                     y = 1:num_rows) %>%
    mutate(x = x * perlin_scale_factor,
           y = y * perlin_scale_factor) %>%
    mutate(angle = ambient::gen_perlin(x, y, seed = perlin_seed,
                                       frequency = perlin_freq))
  
  # normalise angles 
  min_per <- min(long_grid_ff$angle)
  max_per <- max(long_grid_ff$angle)
  
  long_grid_ff <- long_grid_ff %>%
    mutate(angle = (angle - min_per) / (max_per - min_per) * (2*pi-0) + 0)
  
  my_flow_field <- matrix(data = long_grid_ff$angle,
                          ncol = num_cols,
                          nrow = num_rows)
  
  visualised_flow_field <- crossing(
    x = 1:num_cols,
    y = 1:num_rows
  ) %>%
    mutate(angle = map2_dbl(x, y, ~my_flow_field[[.y, .x]])) %>%
    mutate(xend = x + cos(angle) * 0.5,
           yend = y + sin(angle) * 0.5) %>%
    mutate(x_index = x, y_index = y) %>%
    mutate(across(c(x, y, xend, yend), ~ .x * resolution))
  
  list(my_flow_field = my_flow_field, 
       visualised_flow_field = visualised_flow_field)
}

