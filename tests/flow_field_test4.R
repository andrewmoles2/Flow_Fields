library(tidyverse)
library(ambient)
library(MetBrewer)
library(here)

# set up
nx = 200
ny = 200
n_curves = 1500
n_steps = 20
step_length = 1
limit = n_steps * step_length
curve_stroke = 1
curve_alpha = 0.1

pal <- met.brewer(name = "Hokusai1", n = 7)

# mat with nx * ny, vals = angle of perlin noise
noise <- noise_perlin(c(nx, ny), 
                      frequency = 0.01,
                      fractal = "billow", # none, fbm, rigid-multi
                      octaves = 4)

pnt <- array(noise, dim = c(nx, ny),
             dimnames = list(1:nx, 1:ny)) * pi * 4

# mat for lines (start and end points)
#dat <- matrix(nrow = n_curves * n_steps, ncol = 8)
#colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i", "colour")
dat <- matrix(nrow = n_curves * n_steps, ncol = 7)
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i")

# loop for calculations
for (l in 1:n_curves-1) { # loop through lines
  # pick random start points
  x_start = runif(1, min = limit, max = nx - (limit))
  y_start = runif(1, min = limit, max = ny - (limit))
  # get angle from mat (pnt)
  a = pnt[x_start, y_start]
  # get random colour from pal
  colour <- sample(pal, 1)
  
  for (i in 1:n_steps) { # loop through steps for each line
    # calc end point for each step
    x_end = x_start + step_length * cos(a)
    y_end = y_start + step_length * sin(a)
    # write first row of points and angle
    dat[i + l * n_steps, 1] = x_start
    dat[i + l * n_steps, 2] = y_start
    dat[i + l * n_steps, 3] = a
    dat[i + l * n_steps, 4] = x_end
    dat[i + l * n_steps, 5] = y_end
    dat[i + l * n_steps, 6] = l
    dat[i + l * n_steps, 7] = i
#    dat[i + l * n_steps, 8] = colour
    # make starting point from prev end point with new angle
    x_start = x_end
    y_start = y_end
    a = pnt[x_start, y_start]
  }
}

# convert mat to df
dat_df <- as.data.frame(dat)
# change cols to numeric (except colour)
convert <- colnames(dat_df[1:7])
dat_df[, convert] <- sapply(convert, function(x) as.numeric(dat_df[[x]]))

# plot
ggplot(dat_df) +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l),
               size = curve_stroke, alpha = curve_alpha,
               lineend = "round", linejoin = "bevel") + # change those for different effects: lineend = c('round', 'butt', 'square'), linejoin = c('round', 'mitre', 'bevel')) +
  scale_colour_gradient(low = "black", high = "#8CCCE4") +
  scale_fill_gradient(low = "black", high = "#8CCCE4") +
  coord_fixed(xlim = c(limit * 1.5, nx - limit * 1.5), ylim = c(limit * 1.5, ny - limit * 1.5)) + # "crop" to fill the frame
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  filter(l < 150 | l > (n_curves - 150)) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l, size = a),
              alpha = curve_alpha,
               lineend = "round", linejoin = "bevel") + # change those for different effects: lineend = c('round', 'butt', 'square'), linejoin = c('round', 'mitre', 'bevel')) +
  scale_colour_gradient(low = "black", high = "#8CCCE4") +
  scale_fill_gradient(low = "black", high = "#8CCCE4") +
  theme_void() +
  theme(legend.position = "none")

 dat_df %>%
  filter(l < 150 | l > (n_curves - 150)) %>%
  ggplot() +
  geom_point(aes(x = x_start, y = y_start, size = a, colour = l), alpha = 0.05, shape = 19) +
  geom_point(aes(x = x_end, y = y_end, size = a, colour = l), alpha = 0.05, shape = 19) +
  scale_colour_gradient(low = "black", high = "#8CCCE4") +
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  filter(l < 150 | l > (n_curves - 150)) %>%
  ggplot() +
  geom_point(aes(x = x_start, y = y_start, size = a), 
             alpha = 0.55, shape = 21, colour = "orange", fill = "orange") +
  geom_point(aes(x = x_end, y = y_end, size = a, colour = l), 
             alpha = 0.55, shape = 21, colour = "#8CCCE4", fill = "#8CCCE4") +
  theme_void() +
  theme(legend.position = "none")

ggplot(dat_df) +
  geom_line(aes(x = x_start, y = y_end, group = factor(l), colour = i)) +
  scale_colour_gradient(low = "steelblue", high = "orange") +
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  filter(l < 150 | l > (n_curves - 150)) %>%
  filter(i %in% sample(1:20, 20, replace = TRUE)) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l, size = a)) +
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  filter(l < 150 | l > (n_curves - 150)) %>%
  filter(i == 1 | i == 20) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l, size = a)) +
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  filter(i == 20) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = l, size = a)) +
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  filter(l < 150 | l > (n_curves - 150)) %>%
  ggplot() +
  geom_line(aes(x = x_start, y = y_start, group = factor(l), colour = i)) +
  scale_colour_gradient(low = "steelblue", high = "orange") +
  theme_void() +
  theme(legend.position = "none")

dat_df %>%
  #filter(l %in% sample(1:max(dat_df$l), 1000)) %>%
  filter(i <= 7 | i >= 13) %>%
  ggplot() +
  geom_curve(aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
                 colour = i), curvature = 1, show.legend = FALSE,
             size = curve_stroke, alpha = curve_alpha,
             lineend = "round") +
  scale_colour_gradient(low = "#FDA000", high = "#88EC7D") +
  theme_void()


dat_df %>%
  filter(l %in% sample(1:max(dat_df$l), 400)) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = l, colour = i),
               lineend = "round", linejoin = "bevel", show.legend = FALSE,
               size = 5, alpha = 0.75) +
  scale_colour_gradient(low = "#FDA000", high = "#88EC7D") +
  theme_void()
ggsave("flow-fields/tests/flow_test_1.png", dpi = 320, 
       units = "px", width = 3525, height = 3525)


dat_df %>%
  filter(l %in% sample(1:max(dat_df$l), 100)) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = l, colour = colour),
               lineend = "round", linejoin = "bevel", show.legend = FALSE,
               size = 5, alpha = 1) +
  scale_colour_identity() +
  theme_void()


dat_df %>%
  select(a) %>%
  slice_sample(n = 50) %>%
  mutate(a_pos = abs(a))

dat_df$size <- abs(dat_df$a) * 2

dat_df %>%
  filter(l %in% sample(1:max(dat_df$l), 100)) %>%
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, 
                   group = l, colour = colour, size = size),
               lineend = "round", linejoin = "bevel", 
               show.legend = FALSE, alpha = 1) +
  scale_colour_identity() +
  theme_void()


