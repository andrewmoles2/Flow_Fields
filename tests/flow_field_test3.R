# sources
# https://www.williamrchase.com/post/flow-fields-12-months-of-art-september/
# https://cran.r-project.org/web/packages/particles/vignettes/intro.html
# https://www.data-imaginist.com/2020/a-noisy-start/
# https://ambient.data-imaginist.com/reference/index.html
# https://github.com/thomasp85/particles

library(tidyverse)
library(ambient)
library(particles)
library(tidygraph)
library(RColorBrewer)
library(ggraph)

#create noise field
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_simplex(x, y))

#convert noise values to a matrix of angles
field <- as.matrix(grid, x, value = normalize(noise, to = c(-1, 1))) * (2 * pi)

#particle simulation, taken from {particles} vignette
sim <- create_ring(1000) %>%
  simulate(alpha_decay = 0, setup = aquarium_genesis()) %>%
  wield(reset_force, xvel = 0, yvel = 0) %>%
  wield(field_force, angle = field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>%
  evolve(100, record)

ggraph(as_tbl_graph(sim)) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()


traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:1000, 100)

pal <- brewer.pal(9, "Set1")
sizes <- round(seq(0.05, 0.5, length = 30), 2)

traces <- traces %>%
  group_by(particle) %>%
  mutate(
    colour = sample(pal, 1, replace = TRUE))

#plot particle traces
ggplot(traces) +
  geom_path(aes(x, y, group = factor(particle), colour = colour), size = 0.15) +
  theme_void() +
  theme(legend.position = 'none')

sim2 <- create_empty(1000) %>%
  simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>%
  wield(reset_force, xvel = 0, yvel = 0) %>%
  wield(field_force, angle = field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>%
  evolve(100, record)

traces2 <- data.frame(do.call(rbind, lapply(sim2$history, position)))
names(traces2) <- c('x', 'y')
traces2$particle <- rep(1:1000, 100)
traces2$age <- rep(1:1000, 100)
traces2 <- traces2 %>%
  group_by(particle) %>%
  mutate(
    colour = sample(pal, 1, replace = TRUE))

ggplot(traces2) +
  geom_path(aes(x, y, group = particle, colour = colour, alpha = age), size = 0.15) +
  theme_void() +
  theme(legend.position = 'none')

ggplot(traces2) +
  geom_path(aes(x, y, group = particle), size = 0.15) +
  theme_void() +
  theme(legend.position = 'none')

p <- sample(1:max(traces2$particle), 80, replace = FALSE)

traces2 %>%
  filter(particle %in% p) %>%
  ggplot() +
  geom_path(aes(x, y, group = particle, colour = colour, 
                alpha = age, lineend = "round"), size = 3.5) +
  theme_void() +
  theme(legend.position = 'none')


