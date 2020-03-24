## Epidemic model on lattice
## Author: Juha Itkonen @ Robonomist Oy
## 24.3.2020

library(tidyverse)
library(png)
library(Cairo)

set.seed(1235)

n_row <- 64
n_col <- 64
n_size <- n_col*n_row
transition_rate <- 0.1
duration <- 10
kill_rate <- 0.01/duration

## susceptible = 0, infected = 1-, recovered > duration, dead = -1, space = -2
is_infected <- function(a) {
  between(a, 1, duration)
}

model <- function(space = 0) {
  init_infected <- 10
  init_space <- floor(space*n_size)
  periods <- 100

  x <-
  c(rep(0, n_size - init_infected - init_space),
    rep(-2, init_space),
    rep(1, init_infected)) %>%
  sample(n_col*n_row) %>%
  matrix(n_row, n_col)

  spread <-
    tribble(~x, ~y,
            -1, -1,
             0, -1,
             1, -1,
            -1,  0,
             1,  0,
            -1,  1,
             0,  1,
             1,  1)
  n_spread <- nrow(spread)

  inside_range <- function(i, j) {
    between(i, 1, n_row) && between(j, 1, n_col)
  }
  infect <- function() {
    runif(1) < transition_rate
  }
  kill <- function() {
    runif(1) < kill_rate
  }
  step <- function(x) {
    y <- x
    for(i in 1:n_row) {
      for(j in 1:n_col) {
        if(is_infected(x[i, j])) {
          y[i, j] <- ifelse(kill(), -1, x[i, j] + 1)
          for(k in 1:n_spread) {
            p <- i + spread[[k, 1]]
            q <- j + spread[[k, 2]]
            if (inside_range(p, q)) {
              if(x[p, q] == 0) {
                if(infect()) {
                  y[p, q] <- 1
                }
              }
            }
          }
        }
      }
    }
    return(y)
  }

  results <- array(0, c(n_row, n_col, periods))
  results[,,1] <- x
  for(i in 2:periods) {
    results[,,i] <- step(results[,,i-1])
  }
  return(results)
}

download.file("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/236/grinning-face_1f600.png", "s.png")
download.file("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/236/nauseated-face_1f922.png", "i.png")
download.file("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/236/smiling-face-with-sunglasses_1f60e.png", "r.png")
download.file("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/236/skull_1f480.png", "d.png")
susceptible <- readPNG("s.png")
infected <- readPNG("i.png")
recovered <- readPNG("r.png")
dead <- readPNG("d.png")

to_tibble <- function(x)
  tibble(row = c(row(x)),
         col = c(col(x)[, n_col:1]),
         value = c(x))

dir.create("png")

draw <- function(x, i) {
  img <- to_tibble(x)
  filename <- str_c("png/", str_pad(i, 6, pad = "0"), ".png")
  Cairo(file = filename, type = "png",
        ## width = 2160, height = 2160,
        width = 1080, height = 1080,
        bg = "white", canvas = "white")
  par(mar=rep(0,4))
  plot(col ~ row, img,  asp = 1,
       axes = FALSE, xlab = "", ylab = "", type = "n")
  p <- 0.5
  raster <- function(data, emoji) {
    with(data, rasterImage(emoji,
                     row-p, col-p, row+p, col+p,
                     xlab = "", ylab = ""))
  }
  filter(img, value == 0) %>%
    raster(susceptible)
  filter(img, is_infected(value)) %>%
    raster(infected)
  filter(img, value > duration) %>%
    raster(recovered)
  filter(img, value == -1) %>%
    raster(dead)
  text(1, 0, "©Robonomist", adj = 0, col = "gray")
  dev.off()
}

animate <- function(results, filename) {
  for(i in 1:dim(results)[3]) {
    draw(results[,,i], i)
  }
  system(paste("ffmpeg -y -r 10 -f image2 -s 1920x1080 -i png/%06d.png  -vcodec libx264 -crf 25  -pix_fmt yuv420p", filename))
}

animate(model(0), "Ei_tilaa.mp4")
animate(model(0.1), "Tilaa_0.1.mp4")
animate(model(0.3), "Tilaa_0.3.mp4")
animate(model(0.5), "Tilaa_0.5.mp4")


