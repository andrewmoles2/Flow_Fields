start_particles <- function(n_out = 800,
                            flow_field_width = 1000,
                            num_steps = 5,
                            step_lenght = 5,
                            flow_field,
                            resolution_factor = 0.0025) {
  
  df <- tibble::tibble(
    start_x = runif(flow_field_width*-0.1, flow_field_width*1.1, n=n_out),
    start_y = runif(flow_field_width*-0.1, flow_field_width*1.1, n=n_out)
  ) %>%
    mutate(row_num = row_number(),
           resolution = flow_field_width * resolution_factor,
           num_steps = num_steps,
           step_lenght = step_lenght)
  
  df %>%
    pmap_dfr(draw_curve, flow_field = flow_field)
  
}