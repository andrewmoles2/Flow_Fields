library(tidyverse)
library(aRtsy)

colors <- RColorBrewer::brewer.pal(9, "Set1")
background = "#fafafa"
lines = 500 
lwd = 0.05
iterations = 100 
stepmax = 0.01
angles = NULL

grid <- tidyr::crossing(
  x = seq(0, 100, length = 100),
  y = seq(0, 100, length = 100)
) %>%
  mutate(z = 0)



left <- 100 * -0.5
right <- 100 * 1.5
bottom <- 100 * -0.5
top <- 100 * 1.5
ncols <- right - left
nrows <- top - bottom

# noise ----
.noise <- function(dims, n = 100, type = c("artsy-knn", "knn", "svm", "rf"), k = 20, limits = c(0, 1)) {
  type <- match.arg(type)
  if (type == "artsy-knn") {
    if (length(dims) == 1) {
      vec <- expand.grid(limits[1], seq(limits[1], limits[2], length.out = dims))
    } else if (length(dims) == 2) {
      vec <- expand.grid(seq(limits[1], limits[2], length.out = dims[1]), seq(limits[1], limits[2], length.out = dims[2]))
    }
    z <- c_noise_knn(stats::runif(n), stats::runif(n), stats::runif(n), vec[, 1], vec[, 2], k, n)
  } else if (type == "svm") {
    train <- data.frame(
      x = stats::runif(n, limits[1], limits[2]),
      y = stats::runif(n, limits[1], limits[2]),
      z = stats::runif(n, limits[1], limits[2])
    )
    fit <- e1071::svm(formula = z ~ x + y, data = train)
    xsequence <- seq(limits[1], limits[2], length = dims[1])
    ysequence <- seq(limits[1], limits[2], length = dims[2])
    canvas <- expand.grid(xsequence, ysequence)
    colnames(canvas) <- c("x", "y")
    z <- predict(fit, newdata = canvas)
  } else if (type == "knn") {
    train <- data.frame(
      x = stats::runif(n, limits[1], limits[2]),
      y = stats::runif(n, limits[1], limits[2]),
      z = stats::runif(n, limits[1], limits[2])
    )
    fit <- kknn::train.kknn(formula = z ~ x + y, data = train, kmax = k)
    xsequence <- seq(limits[1], limits[2], length = dims[1])
    ysequence <- seq(limits[1], limits[2], length = dims[2])
    canvas <- expand.grid(xsequence, ysequence)
    colnames(canvas) <- c("x", "y")
    z <- predict(fit, newdata = canvas)
  } else if (type == "rf") {
    train <- data.frame(
      x = stats::runif(n, limits[1], limits[2]),
      y = stats::runif(n, limits[1], limits[2]),
      z = stats::runif(n, limits[1], limits[2])
    )
    fit <- randomForest::randomForest(formula = z ~ x + y, data = train)
    xsequence <- seq(limits[1], limits[2], length = dims[1])
    ysequence <- seq(limits[1], limits[2], length = dims[2])
    canvas <- expand.grid(xsequence, ysequence)
    colnames(canvas) <- c("x", "y")
    z <- predict(fit, newdata = canvas)
  }
  return(matrix(z, nrow = dims[1], ncol = dims[2]))
}

# calculate ----
angles <- .noise(
  dims = c(nrows, ncols),
  n = sample(100:300, size = 1),
  type = sample(c("knn", "svm", "rf"), size = 1),
  limits = c(-pi, pi)
)

#plotData <- data.frame(x = numeric(), y = numeric(), z = numeric(), size = numeric(), color = numeric())
plotData <- data.frame(x = numeric(), y = numeric(), color = numeric())


iterate_flow <- function(angles, j, iterations, left, right, top, bottom, step) {
  x <- ceiling(runif(100,left + 1, right - 1))
  y <- ceiling(runif(100,bottom + 1, top - 1))
  m <- nrow(angles)
  n <- ncol(angles)
  #x_val <- vector(length = iterations)
  x_val <- vector()
  #y_val <- vector(length = iterations)
  y_val <- vector()
  z <- integer(length = iterations)
  
  for (i in 1:iterations) {
    col_index <- round(x[length(x) - 1] - left)
    row_index <- round(y[length(y) - 1] - bottom)
    angle <- angles[row_index, col_index]
    xnew <- x[length(x) - 1] + step * cos(angle)
    ynew <- y[length(y) - 1] + step * sin(angle)
    x_val[j] <- xnew
    y_val[j] <- ynew
  }
  
  #z[j] <- j
  df <- data.frame(
    x = x_val,
    y = y_val
    #z
  )
}

iterate_flow(angles, j, iterations, left, right, top, bottom, step)

for (j in 1:lines) {
  step <- stats::runif(1, min = 0, max = 100 * stepmax)
  rows <- iterate_flow(angles, j, iterations, left, right, top, bottom, step)
  rows$color <- sample(colors, size = 1)
  #rows$size <- .bmline(n = nrow(rows), lwd)
  plotData <- rbind(plotData, rows)
}

col_index <- round(x[length(x) - 1] - left)
row_index <- round(y[length(y) - 1] - bottom)
angle <- angles[row_index, col_index]
xnew <- x[length(x) - 1] + step * cos(angle)
ynew <- y[length(y) - 1] + step * sin(angle)
xnew
ynew
if (col_index >= n | col_index <= 0 | row_index >= m | row_index <= 0) {
  paste("issue")
}

z[j] <- j
df <- data.frame(
  x = x_val,
  y = y_val,
  z
)


test <- aRtsy::canvas_flow(colors = colors, lines = 100)
plot_df <- test$data

ggplot(plot_df, mapping = aes(x, y, group = factor(z))) +
  geom_path(size = 5, colour = plot_df$color, lineend = "round") +
  theme_void()

ggplot(plot_df, aes(x, y)) + 
  geom_point(colour = plot_df$color) +
  theme_void()

plot_df %>%
  filter(z <= 5) %>%
  ggplot(aes(x, y, group = factor(z))) +
  geom_path(size = 5, lineend = "round")
