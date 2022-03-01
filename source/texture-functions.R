
source(here::here("source/core-functions.R"))

# the grid is used when constructing the texture 
build_grid <- function(x_limit, y_limit, scale = .1, 
                       base_prob = .4, frequency = .5) {
  
  x_grid <- seq(from = x_limit[1], to = x_limit[2], by = scale * 1.2)
  y_grid <- seq(from = y_limit[1], to = y_limit[2], by = scale)
  gr <- expand_grid(x = x_grid, y = y_grid)
  gr$z <- base_prob
  
  tmp <- curl_noise(
    generator = fracture,
    fractal = clamped,
    noise = gen_simplex,
    octaves = 4,
    x = gr$x, 
    y = gr$y, 
    z = gr$z,
    frequency = frequency,
  )
  
  gr$prob <- tmp$z
  gr$ind <- 1:nrow(gr)
  gr$alpha <- rbeta(nrow(gr), 3, 1)
  gr$scale <- scale
  return(gr)
}

# the modify function is used to construct transformed versions of logo parts
modify_part <- function(data, grid_row, ind, blank_fn) {
  
  # variables
  name <- as.character(rlang::ensym(data))
  x_shift <- grid_row$x
  y_shift <- grid_row$y
  scale <- grid_row$scale
  prob <- grid_row$prob
  alpha <- grid_row$alpha
  
  # I made an arbitrary decision at one point
  alpha <- prob * alpha
  
  # randomly reflect horizontally and/or vertically
  if(runif(1) < .5) data <- data %>% mutate(y = 1 - y)
  if(runif(1) < .5) data <- data %>% mutate(x = 1 - x)
  
  # return null if this is a blank space
  if(blank_fn(x_shift, y_shift)) {
    return(NULL)
  }
  
  data %>% 
    mutate(
      x = x * scale + x_shift,
      y = y * scale + y_shift,
      id = paste0(name, ind),
      alpha = alpha
    )
}


make_texture <- function(x_limit, y_limit, blank_fn) {
  
  # create the components
  trapezoid <- logo_part("trapezoid")
  bracket <- logo_part("bracket")
  
  # define the grid
  gr <- build_grid(x_limit, y_limit)
  
  # for each element of the grid, possibly add modified logo components
  trapezoid_list <- list()
  bracket_list <- list()
  for(i in 1:nrow(gr)) {
    if(runif(1) < gr$z[i]) trapezoid_list[[i]] <- modify_part(trapezoid, gr[i, ], i, blank_fn)
    if(runif(1) < gr$z[i]) bracket_list[[i]] <- modify_part(bracket, gr[i, ], i, blank_fn)
  }
  
  # collapse
  trapezoids <- bind_rows(trapezoid_list)
  brackets <- bind_rows(bracket_list)
  
  return(list(trapezoids = trapezoids, brackets = brackets))

}

