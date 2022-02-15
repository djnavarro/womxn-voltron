# generate Apache Arrow logos programmatically

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(showtext)
#library(magick)


export_logo <- function(plot, path, height = 6, width = 6, background = NULL) {
  ggsave(path, plot, width = width, height = height, dpi = 300, bg = background)
}

generate_trapezoid <- function(vw) {
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
      x = x * vw,
      id = "trapezoid"
    )
}

generate_triangle <- function(v_width) {
  tibble(
    x = v_width * c(0, 1/2, 1/4, 0),
    y = c(1, 1, .5, 1),
    id = "triangle"
  )
}

generate_bracket <- function(vw) {
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
      x = vw * ((.5 + ch) + x * (.5 - ch)),
      y = (.5 + ch * vw / sqrt(2)) + 
        y * (.5 - ch * vw / sqrt(2))
    )
}


generate_slash <- function(vw) {
  wd <- .05
  tribble(
    ~x,    ~y, 
     0,     1, 
    wd,     1, 
  .5 + wd/2,  wd * sqrt(3/2) / vw,
    .5,     0, 
     0,     1
  ) %>% 
    mutate(
      x = x * vw - .6,
      id = "slash"
    )
  
}

generate_tickslash <- function(vw) {
  wd <- .04
  scf <- wd * sqrt(3/2) / vw
  tribble(
    ~x,    ~y, 
    0,     1, 
    wd,     1, 
    .5, 2*scf,
    .5 + wd, 4*scf,
    .5 + 1.5*wd, 3*scf,
    .5,     0, 
    0,     1
  ) %>% 
    mutate(
      x = x * vw - .6,
      id = "slash"
    )
  
}




generate_logotype <- function() {
  tibble(
    x = c(1.5, 1.5),
    y = c(.5, 1),
    text = c("V O L T R O N    D A T A", "W O M X N    A T"),
    font = c("archerus", "archerus"),
    weight = c("bold", "bold"), # <- in the hacked folder this is actually semibold because ggplot2 
    size = c(50, 25), # <- the intent is really a 3:1 ratio in font size
    hjust = c("left", "left"),
    vjust = c("center", "top")
  )
} 


specify_logo_horizontal <- function(colour = "black", background = "white", crop = FALSE) {
  
  # load fonts
  showtext_auto()
  
  font_add(
    family = "archerus", 
    regular = here::here("font", "acherusgrotesque-regular.otf")
  )
  
  # create the components
  v_width <- 1.2
  voltron_text <- generate_logotype()
  trapezoid <- generate_trapezoid(v_width)
  slash <- generate_slash(v_width)
  bracket <- generate_bracket(v_width)
  
  # specify plot limits
  x_limit <- c(-2, 8) # w: 10
  y_limit <- c(-2, 3) # h: 5 
  
  # specify line width
  line_width <- 2

  # construct plot
  pic <- ggplot() +
    geom_polygon(
      data = trapezoid,
      mapping = aes(x, y, group = id, subgroup = part),
      fill = colour,
      colour = colour,
      size = .5
    ) +
    geom_polygon(
      data = slash,
      mapping = aes(x, y, group = id),
      fill = colour,
      colour = colour,
      size = .5
    ) +
    geom_polygon(
      data = bracket,
      mapping = aes(x, y, group = id),
      colour = colour,
      fill = colour,
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
      colour = colour
    ) +
    coord_equal() +
    scale_size_identity() +
    theme_void() +
    theme(panel.background = element_rect(
      fill = background, colour = background
    )) + 
    scale_x_continuous(limits = x_limit, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limit, expand = c(0, 0)) +
    NULL
  
  # invisibly return the ggplot object: note that this object won't
  # render the way you want it to unless you export it in the exact 
  # width, height and dpi settings per export_logo()
  return(invisible(pic))
}


logo_filename <- function(type, colour, background, format) {
  if(is.null(background)) background <- "transparent"
  paste0(
    "voltron-logo_",
    type, "_",
    colour, "-txt_",
    background, "-bg.",
    format
  )
}

create_logo_horizontal <- function(dir, colour, background, format = "png") {
  pic <- specify_logo_horizontal(colour, background, crop = crop)
  fname <- logo_filename("horizontal", colour, background, format)
  export_logo(
    plot = pic,
    path = file.path(dir, fname), 
    background = background, 
    height = 6,
    width = 12
  )
}

create_logo_horizontal(
  dir = here::here("img"),
  colour = "black",
  background = "white"
)




