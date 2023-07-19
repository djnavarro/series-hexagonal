library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(tictoc)
library(ggthemes)
library(here)

Rcpp::sourceCpp(here("source", "grow-polygon-02.cpp"))

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

smudged_hexagon <- function(seed, noise1 = 0.1, noise2 = 1.5, noise3 = 0.3) {
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
      iterations = 100, 
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
    
    for(j in 1:10) {
      base_j <- base_i |> 
        grow_polygon_l(
          iterations = 100, 
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

seeds <- 1800:1899
for(seed in seeds) {
  
  set.seed(seed)
  cat("generating image:", seed, "\n")
  
  
  n_hex <- 3
  shades <- sample_canva3(n = n_hex + 1)
  dat <- list()
  
  for(i in 1:n_hex) {
    dat[[i]] <- smudged_hexagon(seed = sample(10000, 1)) |>
      mutate(
        fill = shades[i], 
        s = if_else(i == 1, 2, runif(n = 1, min = 1, max = 2)),
        x = x * s + if_else(i == 1, 0, rnorm(1, mean = 0, sd = 1)), 
        y = y * s + if_else(i == 1, 0, rnorm(1, mean = 0, sd = 1)),
        height = -i,
        id = paste0("hex", i, "_id", id)
      )
  }
  
  dat <- bind_rows(dat) |>
    arrange(height) |>
    group_by(id) |> 
    mutate(dilution = s / (max(x) - min(x)) / (max(y) - min(y))) |>
    ungroup() |>
    mutate(dilution = dilution / max(dilution))
  
  pic <- ggplot(dat, aes(x, y, group = id, fill = fill, alpha = .01 * dilution)) +
    geom_polygon(colour = NA, show.legend = FALSE) + 
    scale_fill_identity() +
    scale_alpha_identity() +
    coord_equal(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
    theme_void()
  
  fname <- paste0("smudged-hexagon_011_", seed, ".png")
  
  ggsave(
    filename = here("output", fname), 
    plot = pic,
    width = 4000,
    height = 4000,
    units = "px",
    dpi = 300,
    bg = shades[n_hex + 1]
  )
  
}
