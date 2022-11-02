library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tictoc)
library(ggthemes)
library(here)

Rcpp::sourceCpp(here("source", "grow-polygon.cpp"))

grow_polygon_l <- function(polygon, iterations, noise, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  polygon <- grow_polygon(polygon, iterations, noise) |>
    as_tibble() |>
    arrange(position) |>
    select(x, y, seg_len)
  return(polygon)
}

grow_multipolygon_l <- function(base_shape, n, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  polygons <- list()
  for(i in 1:n) {
    polygons[[i]] <- grow_polygon_l(base_shape, ...) |>
      mutate(id = i)
  }
  polygons <- bind_rows(polygons)
  polygons
}

show_multipolygon <- function(polygon, fill, alpha = .02, ...) {
  ggplot(polygon, aes(x, y, group = id)) +
    geom_polygon(colour = NA, alpha = alpha, fill = fill, ...) + 
    coord_equal() + 
    theme_void()
}

edge_length <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

smudged_hexagon <- function(seed, noise1 = 0, noise2 = 2, noise3 = 0.5) {
  set.seed(seed)
  
  # define hexagonal base shape
  theta <- (0:6) * pi / 3
  hexagon <- tibble(
    x = sin(theta),
    y = cos(theta),
    seg_len = edge_length(x, y, lead(x), lead(y))
  )
  hexagon$seg_len[7] <- 0
  base <- hexagon |> 
    grow_polygon_l(
      iterations = 60, 
      noise = noise1
    )
  
  # define intermediate-base-shapes in clusters
  polygons <- list()
  ijk <- 0
  for(i in 1:3) {
    base_i <- base |> 
      grow_polygon_l(
        iterations = 100, 
        noise = noise2
      )
    
    for(j in 1:3) {
      base_j <- base_i |> 
        grow_polygon_l(
          iterations = 50, 
          noise = noise2
        )
      
      # grow n polygons per intermediate-base
      for(k in 1:10) {
        ijk <- ijk + 1
        polygons[[ijk]] <- base_j |>
          grow_polygon_l(
            iterations = 800, 
            noise = noise3
          ) |>
          mutate(id = ijk)
      }
    }
  }
  
  # return as data frame
  bind_rows(polygons)
}

sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

sample_canva3 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  max_pal <- max(colorir::colores$palette_index)
  pal_ind <- sample(max_pal, 1)
  shades <- colorir::colores$colour[colorir::colores$palette_index == pal_ind]
  shades |>
    (\(x) colorRampPalette(x)(n))()  
}

seeds <- 1600:1699
for(seed in seeds) {
  
  set.seed(seed)
  cat("generating image:", seed, "\n")
  
  
  n_hex <- 20
  shades <- sample_canva3(n = n_hex)
  dat <- list()
  
  for(i in 1:n_hex) {
    dat[[i]] <- smudged_hexagon(seed = sample(10000, 1)) |>
      mutate(
        fill = shades[i], 
        s = rbeta(n = 1, 1, 4) * 2,
        id = paste0("sz", round(s*100), "_hex", i, "_id", id), 
        x = (x + runif(1, min = -2, max = 2)) * s, 
        y = (y + runif(1, min = -2, max = 2)) * s
      )
  }
  
  dat <- bind_rows(dat) |>
    arrange(desc(id)) |>
    group_by(id) |> 
    mutate(size = (max(x) - min(x)) * (max(y) - min(y)) / s)
  
  pic <- ggplot(dat, aes(x, y, group = id, fill = fill, alpha = .1/size)) +
    geom_polygon(colour = NA, show.legend = FALSE) + 
    scale_fill_identity() +
    scale_alpha_identity() +
    coord_equal(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
    theme_void()
  
  fname <- paste0("smudged-hexagon_009_", seed, ".png")
  
  ggsave(
    filename = here("output", fname), 
    plot = pic,
    width = 4000,
    height = 4000,
    units = "px",
    dpi = 300,
    bg = colorspace::lighten(shades[1], amount = .2)
  )
  
}
