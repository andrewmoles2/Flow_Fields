# libraries
library(tidyverse)
library(ambient)
library(gganimate)
library(here)
library(MetBrewer)
library(RColorBrewer)

source(file = here("functions","flow_data_setup.R"))

# test output - should see a data frame with 9 cols
(test <- flow_setup())

# plot set-up
curve_stroke <- 1
curve_alpha <- 0.1
limit <- max(test$i)
limit_x <- round(max(test$x_start))
limit_y <- round(max(test$y_start))

# use the test set to plot - need to use either geom_segment or geom_curve ----
# using coord cartesian to fit plotting area
test %>% 
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l),
               size = curve_stroke, alpha = curve_alpha,
               lineend = "round", linejoin = "bevel") + # change those for different effects: lineend = c('round', 'butt', 'square'), linejoin = c('round', 'mitre', 'bevel')) +
  scale_colour_gradient(low = "black", high = "#8CCCE4") +
  scale_fill_gradient(low = "black", high = "#B75347") +
  coord_cartesian(xlim = c(limit * 1.5, limit_x - limit * 1.5), ylim = c(limit * 1.5, limit_y - limit * 1.5)) + # "crop" to fill the frame
  theme_void() +
  theme(legend.position = "none")

streams <- test %>% 
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l),
               size = curve_stroke, alpha = curve_alpha,
               lineend = "round", linejoin = "bevel") + # change those for different effects: lineend = c('round', 'butt', 'square'), linejoin = c('round', 'mitre', 'bevel')) +
  scale_colour_gradient(low = brewer.pal(n=9, "Blues")[1], high = brewer.pal(n=9, "Blues")[9]) +
  scale_fill_gradient(low = brewer.pal(n=9, "Blues")[1], high = brewer.pal(n=9, "Blues")[9]) +
  coord_cartesian(xlim = c(limit * 1.5, limit_x - limit * 1.5), ylim = c(limit * 1.5, limit_y - limit * 1.5)) + # "crop" to fill the frame
  theme_void() +
  theme(legend.position = "none") 
streams

ggsave(filename = "outputs/streams.png", plot = streams,
       dpi = 600, units = "px", width = 3525, height = 3525)

# more tests with different types of noise: perlin, worley, cubic, simplex, or value ----
# also different types of fractal: none, fbm, billow, rigid-multi
flow_setup(noise = "cubic", n_curves = 500, n_steps = 15,
            octaves = 2, fractal = "none", frequency = 0.005) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = l, colour = i),
               lineend = "round", linejoin = "bevel", show.legend = FALSE,
               size = 5, alpha = 0.75) +
  scale_colour_gradient(low = "#FDA000", high = "#88EC7D") +
  theme_void()

flow_setup(noise = "value", n_curves = 500, n_steps = 20, step_length = 2) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = l, colour = i),
               lineend = "round", linejoin = "round", show.legend = FALSE,
               size = 5, alpha = 0.95) +
  scale_colour_gradient(low = "#FDA000", high = "#88EC7D") +
  theme_void()

# animation of the flow ----
anim_flow <- flow_setup(noise = "simplex", n_curves = 7500, n_steps = 50, step_length = 1) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = l, colour = i),
               lineend = "round", linejoin = "round", show.legend = FALSE,
               size = 5, alpha = 0.95) +
  scale_colour_gradient(low = "#FDA000", high = "#88EC7D") +
  theme_void() +
  transition_time(i)

gganimate::animate(anim_flow, fps = 20, nframes = 150, 
                   end_pause = 10, rewind = TRUE)

gganimate::anim_save("outputs/running_to_streams.gif")

# test using the size and colour variables ----
# works best with ncurve < 1000
# note: that it takes ages with ncurve > 10,000

paint_strokes <- flow_setup(n_curves = 150, noise = "cubic", seed = 12, n_steps = 25) %>% 
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, 
                   group = l, colour = colour, size = size),
               lineend = "round", linejoin = "bevel", 
               show.legend = FALSE, alpha = 1) +
  scale_colour_identity() +
  theme_void()
paint_strokes

flow_spiral <- flow_setup(n_curves = 150, noise = "cubic", seed = 19, n_steps = 20) %>% 
  ggplot() +
  geom_curve(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, 
                   group = l, colour = colour, size = size),
               lineend = "round", show.legend = FALSE, 
             alpha = 1, curvature = 2) +
  scale_colour_identity() +
  theme_void()
flow_spiral
  
ggsave(filename = "outputs/paint_strokes.png", plot = paint_strokes,
       dpi = 500, units = "px", width = 3525, height = 3525)
ggsave(filename = "outputs/flow_spiral.png", plot = flow_spiral,
       dpi = 500, units = "px", width = 3525, height = 3525)

# what if I want to sample from my main data I made earlier? ----
# we use sample or runif to make a random set of indices based on the l variable (n loops)
sample(1:max(test$l), 15)
round(runif(15, min = 1, max = max(test$l)))

test |> subset(l %in% sample(1:max(test$l), 15)) |> head()
test |> subset(l %in% round(runif(15, min = 1, max = max(test$l)))) |> head()

# try using geom_path/line ----
# making waves
wave_01 <- flow_setup(n_curves = 3000, noise = "cubic", fractal = 'none', octaves = 0.75,
           seed = 19, n_steps = 25, step_length = 3, pal = brewer.pal(9, "PuBuGn")) %>%
  ggplot(aes(x = x_start, y = y_start, group = l, 
             colour = colour)) +
  geom_path(size = 0.9, alpha = 0.8, lineend = "round") +
  scale_colour_identity() +
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = brewer.pal(9, "Pastel1")[5]),
    plot.background = element_rect(fill =  brewer.pal(9, "Pastel1")[5])
  )

wave_02 <- flow_setup(n_curves = 700, noise = "cubic", fractal = 'none', 
           octaves = 0.75, seed = 12, n_steps = 16, step_length = 4, 
           pal = c('#05668d','#04738f','#028090','#00a896','#01b698','#02c39a','#f0f3bd','#f1f4c3')) %>%
  ggplot(aes(x = x_start, y = y_start, group = l, 
             colour = colour)) +
  geom_path(size = 0.75, alpha = 0.75, lineend = "round") +
  scale_colour_identity() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = brewer.pal(9, "Pastel1")[1]),
    plot.background = element_rect(fill =  brewer.pal(9, "Pastel1")[1])
  )

wave_01
wave_02

ggsave(filename = "outputs/waves_01.png", plot = wave_01,
       dpi = 500, units = "px", width = 3525, height = 3525)
ggsave(filename = "outputs/wave_02.png", plot = wave_02,
       dpi = 500, units = "px", width = 3525, height = 3525)

# annimated version
moving_wave <- flow_setup(n_curves = 200, noise = "cubic", fractal = 'none', octaves = 0.75,
           seed = 19, n_steps = 26, step_length = 3, pal = brewer.pal(9, "PuBuGn")) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, 
                   group = l, colour = colour, size = size),
               lineend = "round", linejoin = "bevel", 
               show.legend = FALSE, alpha = 1) +
  scale_colour_identity() +
  theme_void() +
  transition_time(i) +
  shadow_trail(0.2, alpha = 0.3, 
               max_frames = 10, shape = 2)

gganimate::animate(moving_wave, fps = 25, nframes = 150, 
                   end_pause = 5, rewind = FALSE)

gganimate::anim_save("outputs/moving_wave.gif")


# larger full flow with lines ----
streams_02 <- test |> 
  ggplot(aes(x = x_start, y = y_start,group = l, 
             colour = colour)) +
  geom_path(size = 0.55, alpha = 0.8, 
            lineend = "round", linejoin = "round", linemitre = 3) +
  scale_colour_identity() +
  theme_void()
streams_02

ggsave(filename = "outputs/streams_02.png", plot = streams_02,
       dpi = 500, units = "px", width = 3525, height = 3525)

