library(ggplot2)
library(tidyverse)
library(noise)

# set a random seed for reproducibility
set.seed(123)

# define constants
TWO_PI <- 2 * pi
N_ARCS <- 100
WIDTH <- 800
HEIGHT <- 800
MIN_DEPTH <- 0
MAX_DEPTH <- 4

# define a recursive function to draw arcs
recursive_arc <- function(x, y, a1, a2, r1, r2, depth, palette) {
  if (depth < MIN_DEPTH) return(NULL)
  if (a1 > a2) {
    tmp <- a1
    a1 <- a2
    a2 <- tmp
  }
  if (r1 > r2) {
    tmp <- r1
    r1 <- r2
    r2 <- tmp
  }
  rsx <- runif(1, 0, 1000)
  rsy <- runif(1, 0, 1000)
  t <- noise::rasterNoise(rsx, rsy, n = 1, freq = seq(0, 5, length.out = MAX_DEPTH+1)[depth+1] / 800, octaves = 1, lacunarity = 2, persistence = 0.5)
  t <- t[[1]] / max(t[[1]])
  na <- a1 + (sin(rsx + y / 20 + ((t * TWO_PI) / 4) * depth) / 2 + 0.5) * (a2 - a1)
  nr <- r1 + (cos(rsy + x / 20 + ((t * TWO_PI) / 4) * depth) / 2 + 0.5) * (r2 - r1)
  if (depth == MAX_DEPTH) {
    arc1 <- data.frame(x = x, y = y, start_angle = a1 + a_offset, end_angle = na - a_offset, min_d = r1 + r_offset, max_d = nr - r_offset)
    arc2 <- data.frame(x = x, y = y, start_angle = na + a_offset, end_angle = a2 - a_offset, min_d = r1 + r_offset, max_d = nr - r_offset)
    arc3 <- data.frame(x = x, y = y, start_angle = a1 + a_offset, end_angle = na - a_offset, min_d = nr + r_offset, max_d = r2 - r_offset)
    arc4 <- data.frame(x = x, y = y, start_angle = na + a_offset, end_angle = a2 - a_offset, min_d = nr + r_offset, max_d = r2 - r_offset)
    return(list(arc1, arc2, arc3, arc4))
  } else {
    return(list(recursive_arc(x, y, a1, na, r1, nr, depth + 1, palette),
                recursive_arc(x, y, na, a2, r1, nr, depth + 1, palette),
                recursive_arc(x, y, a1, na, nr, r2, depth + 1, palette),
                recursive_arc(x, y, na, a2, nr, r2, depth + 1, palette)))
  }
}

# set up
