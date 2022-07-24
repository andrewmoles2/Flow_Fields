# enforce a minimum distance between curves. At each step of a curve, check if any other existing curves are too close, and if so, stop
# https://web.cs.ucdavis.edu/~ma/SIGGRAPH02/course23/notes/papers/Jobard.pdf
# https://stackoverflow.com/questions/20982635/identify-points-within-specified-distance-in-r

# id points that are near (doesn't work on larger mats...)
set.seed(1234)
x <- sample(1:1000, 500)
y <- sample(1:1000, 500)
M <- cbind(x, y)

DM <- as.matrix(dist(M))
neighbors <- which(DM < 15, arr.ind = TRUE)
neighbors <- neighbors[neighbors[, 1] != neighbors[, 2]]

plot(M)
points(M[neighbors, ], col = "red")

# using FNN (ran loop in flow_field_test4 first) ----
# set up
nx = 200
ny = 200
n_curves = 150
n_steps = 20
step_length = 1
limit = n_steps * step_length
curve_stroke = 1
curve_alpha = 0.1

# mat with nx * ny, vals = angle of perlin noise
noise <- noise_perlin(c(nx, ny), 
                      frequency = 0.01,
                      fractal = "billow", # none, fbm, rigid-multi
                      octaves = 4)

pnt <- array(noise, dim = c(nx, ny),
             dimnames = list(1:nx, 1:ny)) * pi * 4

# mat for lines (start and end points)
dat <- matrix(nrow = n_curves * n_steps, ncol = 7)
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i")

# loop for calculations
for (l in 1:n_curves-1) { # loop through lines
  # pick random start points
  x_start = runif(1, min = limit, max = nx - (limit))
  y_start = runif(1, min = limit, max = ny - (limit))
  # get angle from mat (pnt)
  a = pnt[x_start, y_start]
  
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

library(FNN)

knnx.dist(dat[, 1:2], 
          matrix(c(40, 100), ncol = 2), 
          k = 2)

knnx.index(dat[, 1:2], 
          matrix(c(40, 100), ncol = 2), 
          k = 2) 

dat[c(28819, 14168),]

knnDist <- 2
if (knnx.dist(dat[, 1:2],matrix(c(x_start, y_start), ncol = 2),k = 1) < knnDist) {
  print("Break for loop")
} else {
  print("Keep loop going")
}

## testing out knn loop breaker 
dat <- matrix(nrow = n_curves * n_steps, ncol = 7)
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i")
knnDist <- 2
dat[is.na(dat)] = 0

# loop for calculations
for (l in 1:n_curves-1) { # loop through lines
  # pick random start points
  x_start = runif(1, min = limit, max = nx - (limit))
  y_start = runif(1, min = limit, max = ny - (limit))
  # get angle from mat (pnt)
  a = pnt[x_start, y_start]
  
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
    # make starting point from prev end point with new angle
    x_start = x_end
    y_start = y_end
    a = pnt[x_start, y_start]
  }
}
dat_df <- as.data.frame(dat)

ggplot(dat_df, aes(x_start, y_start, group = l)) + geom_path()

# make vector of bool if knn dist < 2. Then filter on that. 
knnx.dist(dat_df[, 1:2], dat_df[500, 1:2], k = 21) < knnDist
test <- knnx.dist(dat_df[, 1:2], dat_df[500, 1:2], k = 21)[21] < knnDist

test <- vector("double", nrow(dat_df[ ,1:2]))

for (k in seq_along(1:nrow(dat_df))) {
  test[k] <- knnx.dist(dat_df[, 1:2], dat_df[k, 1:2], k = 12, algorithm = "cover_tree")[8] < knnDist
}
# algorithm=c("kd_tree", "cover_tree", "CR", "brute")
dat_df$knn <- test

dat_df %>%
  filter(knn != 1) %>%
  ggplot(aes(x_start, y_start, group = l)) + geom_path()


