# generate Womxn at Voltron Data textures

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(showtext)
#library(magick)

sys_id <- "04"


export_logo <- function(plot, path, height = 6, width = 6, background = NULL) {
  ggsave(path, plot, width = width, height = height, dpi = 96, bg = background)
}

generate_trapezoid <- function() {
  vw <- 1.2
  wd <- .075
  tribble(
    ~x,    ~y,   ~part,
    0,     1, "outer",
    .5,     1, "outer",
    .75,    .5, "outer",
    .5,     0, "outer",
    0,     1, "outer",
    .5,  1-wd, "inner",
    .75-wd/2,    .5, "inner",
    .5,    wd, "inner",
    .25+wd/2,    .5, "inner",
    .5,  1-wd, "inner"
  ) %>% 
    mutate(
      x = x * vw + .2,
      id = "trapezoid"
    )
}

generate_triangle <- function() {
  vw <- 1.2
  tibble(
    x = vw * c(0, 1/2, 1/4, 0) + .2,
    y = c(1, 1, .5, 1),
    id = "triangle"
  )
}

generate_bracket <- function() {
  vw <- 1.2
  wd <- .08 * 2
  l <- wd * sin(pi/3)
  s <- wd * cos(pi/3)
  tribble(
    ~x,      ~y,
    -s,       1,
    1,       1,
    .5,       0,
    .5-s,       l,
    1-wd-s,   1-l,
    0, 1-l,
    -s, 1
  ) %>% 
    mutate(
      id = "bracket",
      ch = vw/2 * .5,
      x = vw * ((.5 + ch) + x * (.5 - ch)) + .2,
      y = (.5 + ch * vw / sqrt(2)) + 
        y * (.5 - ch * vw / sqrt(2))
    )
}


generate_slash <- function() {
  vw <- 1.2 
  wd <- .01
  tribble(
    ~x,    ~y, 
    0,     1, 
    wd,     1, 
    .5 + wd/2,  wd * sqrt(3/2) / vw,
    .5,     0, 
    0,     1
  ) %>% 
    mutate(
      x = x * vw - .4,
      id = "slash"
    )
  
}

generate_logotype <- function() {
  tibble(
    x = c(2, 2),
    y = c(.5, 1),
    text = c("V O L T R O N   D A T A", "W O M X N    A T"),
    font = c("archerus", "archerus"),
    weight = c("bold", "italic"),  # ... these are lies!
    size = c(20, 10),
    hjust = c("left", "left"),
    vjust = c("center", "top")
  )
} 

generate_logotype <- function() {
  tibble(
    x = c(2, 2),
    y = c(.5, 1),
    text = c("V O L T R O N   D A T A", "W O M X N    A T"),
    font = c("archerus", "archerus"),
    weight = c("bold", "italic"),  # ... these are lies!
    size = c(20, 10),
    hjust = c("left", "left"),
    vjust = c("center", "top")
  )
} 

read_shade <- function(colour) {
  colour_code <- colour
  if(colour_code == "primarygreen") colour_code <- "#005050"
  if(colour_code == "primarygrey") colour_code <- "#d9d8d6"
  return(colour_code)
}

specify_texture <- function(colour = "black", background = "white", crop = FALSE, seed = 1) {
  
  set.seed(seed)

  # load fonts
  showtext_auto()
  
  acherus <- function(name) {
    here::here("font", paste0("acherusgrotesque-", name, ".otf"))
  }
  
  # there may be some lies here...
  font_add(
    family = "archerus", 
    regular = acherus("regular"),
    bold = acherus("semibold"), 
    italic = acherus("light")
  )
  
  # create the components
  base_trapezoid <- generate_trapezoid()
  base_slash <- generate_slash()
  base_bracket <- generate_bracket()
  voltron_text <- generate_logotype()
  
  # create the components
  trapezoid <- generate_trapezoid()
  slash <- generate_slash()
  bracket <- generate_bracket()
  
  trapezoid_list <- list()
  slash_list <- list()
  bracket_list <- list()
  
  modify <- function(data, x_shift, y_shift, scale, alpha, ind) {
    name <- as.character(rlang::ensym(data))
    
    if(runif(1) < .5) data <- data %>% mutate(y = 1 - y)
    if(runif(1) < .5) data <- data %>% mutate(x = 1 - x)
    
    data %>% 
      mutate(
        x = x * scale + x_shift,
        y = y * scale + y_shift,
        id = paste0(name, ind),
        alpha = alpha
      )
  }

  # specify plot limits
  x_limit <- c(-4, 12) # w: 16
  y_limit <- c(-3.5, 4.5) # h: 8
  
    
  p <- .3
  ind <- 0
  scale <- .2
  for(x_shift in seq(from = x_limit[1], to = x_limit[2], by = scale * 1.2)) {
    for(y_shift in seq(from = y_limit[1], to = y_limit[2], by = scale)) {
      ind <- ind + 1
      alpha <- rbeta(1, 1, 6)
      if(runif(1) < p) trapezoid_list[[ind]] <- modify(trapezoid, x_shift, y_shift, scale, alpha, ind)
      if(runif(1) < p) bracket_list[[ind]] <- modify(bracket, x_shift, y_shift, scale, alpha, ind)
    }
  }
  
  trapezoid <- bind_rows(trapezoid_list)
  slash <- bind_rows(slash_list)
  bracket <- bind_rows(bracket_list)
  
  # specify colours
  colour_code <- read_shade(colour)
  background_code <- read_shade(background)
  
  # construct plot
  pic <- ggplot() +
    
    # background
    geom_polygon(
      data = trapezoid,
      mapping = aes(x, y, group = id, subgroup = part, alpha = alpha),
      fill = colour_code,
      colour = colour_code,
      size = .1, 
      show.legend = FALSE
    ) +
    geom_polygon(
      data = bracket,
      mapping = aes(x, y, group = id, alpha = alpha),
      colour = colour_code,
      fill = colour_code,
      size = .1,
      show.legend = FALSE
    ) +
    
    # foreground
    geom_polygon(
      data = base_trapezoid,
      mapping = aes(x, y, group = id, subgroup = part),
      fill = colour_code,
      colour = colour_code,
      size = .5
    ) +
    geom_polygon(
      data = base_slash,
      mapping = aes(x, y, group = id),
      fill = colour_code,
      colour = colour_code,
      size = .1
    ) +
    geom_polygon(
      data = base_bracket,
      mapping = aes(x, y, group = id),
      colour = colour_code,
      fill = colour_code,
      size = .5,
    ) +
    geom_text(
      data = voltron_text,
      mapping = aes(
        x, y,
        label = text,
        family = font,
        size = size,
        fontface = weight,
        hjust = hjust,
        vjust = vjust
      ),
      colour = colour_code
    ) +
    
    coord_equal() +
    scale_size_identity() +
    scale_alpha_identity() + 
    theme_void() +
    theme(panel.background = element_rect(
      fill = background_code, colour = background_code
    )) + 
    scale_x_continuous(limits = x_limit, expand = c(0, 0), oob = scales::oob_keep) +
    scale_y_continuous(limits = y_limit, expand = c(0, 0), oob = scales::oob_keep) +
    NULL
  
  # invisibly return the ggplot object: note that this object won't
  # render the way you want it to unless you export it in the exact 
  # width, height and dpi settings per export_logo()
  return(invisible(pic))
}


texture_filename <- function(type, colour, background, format, seed) {
  if(is.null(background)) background <- "transparent"
  paste0(
    "texture_",
    type, "_",
    colour, "-txt_",
    background, "-bg_",
    seed, ".", 
    format
  )
}


create_texture <- function(dir, colour, background, format = "png", seed = 1) {
  pic <- specify_texture(colour, background, crop = crop, seed = seed)
  fname <- texture_filename(sys_id, colour, background, format, seed)
  export_logo(
    plot = pic,
    path = file.path(dir, fname), 
    background = read_shade(background), 
    height = 12,
    width = 24
  )
}

create_texture(
  dir = here::here("img"),
  colour = "white",
  background = "primarygreen",
  seed = 1002
)




