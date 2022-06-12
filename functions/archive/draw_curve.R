draw_curve <- function(start_x, 
                       start_y, 
                       row_num, 
                       flow_field, 
                       resolution, 
                       left_x = 1 * resolution, 
                       bot_y  = 1 * resolution,
                       num_steps,   
                       step_length) { 
  
  x_container <- vector("numeric", num_steps+1)
  y_container <- vector("numeric", num_steps+1)
  
  x_container[1] <- start_x
  y_container[1] <- start_y
  
  
  # grid dimension range
  x_dim_range <- 1:ncol(flow_field) 
  y_dim_range <- 1:nrow(flow_field)
  
  # With the rest of num_steps, move through the flow field, 
  # Each time, stepping towards the closest angle we can grab.
  for (i in 1:num_steps) {
    
    next_step <- step_into_next_curve_segment( 
      start_x     = x_container[i], 
      start_y     = y_container[i],
      left_x      = left_x,
      bot_y       = bot_y,
      resolution  = resolution,
      x_dim_range = x_dim_range,
      y_dim_range = y_dim_range,
      flow_field  = flow_field,
      step_length = step_length
    )
    
    x_container[i+1] <- next_step$x
    y_container[i+1] <- next_step$y
    
  }
  
  tibble::tibble(
    x = x_container,
    y = y_container,
    row_num = row_num 
  ) %>%               
    dplyr::mutate(plot_order = dplyr::row_number())  
}

