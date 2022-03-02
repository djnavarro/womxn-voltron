
# make sure we have necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(showtext)
library(ambient)

# a convenient wrapper
export_logo <- function(plot, path, height = 6, width = 6, background = NULL) {
  ggsave(path, plot, width = width, height = height, dpi = 96, bg = background)
}

# voltron colours
voltron_palette <- c(
  "green" = "#005050",
  "grey" = "#d9d8d6",
  "black" = "#000000",
  "white" = "#ffffff"
)

# themes based on the colours
visual_themes <- tribble(
  ~colour,  ~background,
  "black",       "grey",
  "green",      "black",
  "green",       "grey",
  "grey",       "green"
)

# data needed to specify the logo
generate_logo <- function(vw) {
  
  # constants
  vw <- 1.2
  wd <- .075
  
  # the trapezoidal shape with the hollow diamond inside it that
  # forms the bulk of the Voltron Data logo
  trapezoid <- tribble(
    ~x,    ~y,   ~part,     ~object,
    0,     1, "outer", "trapezoid", # outer shape of trapezoid
    .5,     1, "outer", "trapezoid",
    .75,    .5, "outer", "trapezoid",
    .5,     0, "outer", "trapezoid",
    0,     1, "outer", "trapezoid",
    .5,  1-wd, "inner", "trapezoid", # inner diamond of trapezoid
    .75-wd/2,    .5, "inner", "trapezoid",
    .5,    wd, "inner", "trapezoid",
    .25+wd/2,    .5, "inner", "trapezoid",
    .5,  1-wd, "inner", "trapezoid"
  ) %>% 
    mutate(x = x * vw)
  
  # constants
  wd <- .16
  l <- wd * sin(pi/3)
  s <- wd * cos(pi/3)
  ch <- vw/2 * .5
  
  # the bracket is the oriented chevron that completes the
  # Voltron data logo
  bracket <- tribble(
    ~x,    ~y,     ~part,   ~object,
    -s,     1, "bracket", "bracket",
    1,     1, "bracket", "bracket",
    .5,     0, "bracket", "bracket",
    .5-s,     l, "bracket", "bracket",
    1-wd-s,   1-l, "bracket", "bracket",
    0,   1-l, "bracket", "bracket",
    -s,     1, "bracket", "bracket"
  ) %>% 
    mutate(
      x = vw * ((.5 + ch) + x * (.5 - ch)),
      y = (.5 + ch * vw / sqrt(2)) + 
        y * (.5 - ch * vw / sqrt(2))
    )
  
  # constants
  wd <- .01
  
  # the slash is the additional component that is specific
  # to the Womxn at Voltron Data group, added to give the
  # hint of a letter W rather than a letter V
  slash <- tribble(
    ~x,                   ~y,   ~part, ~object,
    0,                    1, "slash", "slash", 
    wd,                    1, "slash", "slash", 
    .5 + wd/2,  wd * sqrt(3/2) / vw, "slash", "slash",
    .5,                    0, "slash", "slash", 
    0,                    1, "slash", "slash"
  ) %>% 
    mutate(x = x * vw - .6)
  
  return(bind_rows(trapezoid, bracket, slash))
  
}

# convenience function 
logo_part <- function(part) {
  generate_logo() %>% 
    filter(object == {{part}})
}

# data needed to specify the logo text
generate_logotype <- function(message = "", position = "") {
  
  # load fonts
  showtext_auto()
  
  # helper function
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
  
  # this is the text for womxn at voltron data
  logotext <- tibble(
    x = c(2.4, 2.4),
    y = c(.5, 1),
    text = c("V O L T R O N   D A T A", 
             "W O M X N    A T"),
    font = c("archerus"),
    weight = c("bold", "italic"),  # ...lies!
    size = c(20, 10),
    hjust = c("left", "left"),
    vjust = c("center", "top")
  )
  
  # places Ada Lovelace underneath the Voltron Data logo
  lovelace_below <- tibble(
    x = c(4, 4),
    y = c(-.6, -1.1),
    text = c("T H E   A D A   L O V E L A C E   M E E T I N G", 
             "8   M A R C H   2 0 2 2"),
    font = c("archerus"),
    weight = c("bold", "italic"),  # ...lies!
    size = c(12, 10),
    hjust = c("middle", "middle"),
    vjust = c("center", "center")
  )
  
  # places Ada Lovelace to the right of Voltron Data logo
  lovelace_right <- tibble(
    x = c(15, 15),
    y = c(1, .59),
    text = c("T H E   A D A   L O V E L A C E   M E E T I N G", 
             "8   M A R C H   2 0 2 2"),
    font = c("archerus"),
    weight = c("italic", "italic"),  # ...lies!
    size = c(10, 10),
    hjust = c("right", "right"),
    vjust = c("top", "center")
  )
  
  
  # places IWD underneath the Voltron Data logo
  iwd_below <- tibble(
    x = c(4, 4),
    y = c(-.6, -1.1),
    text = c("I N T E R N A T I O N A L   W O M E N ' S   D A Y", 
             "8   M A R C H   2 0 2 2"),
    font = c("archerus"),
    weight = c("bold", "italic"),  # ...lies!
    size = c(12, 10),
    hjust = c("middle", "middle"),
    vjust = c("center", "center")
  )
  
  # places IWD to the right of Voltron Data logo
  iwd_right <- tibble(
    x = c(15, 15),
    y = c(1, .59),
    text = c("I N T E R N A T I O N A L   W O M E N ' S   D A Y", 
             "8   M A R C H   2 0 2 2"),
    font = c("archerus"),
    weight = c("italic", "italic"),  # ...lies!
    size = c(10, 10),
    hjust = c("right", "right"),
    vjust = c("top", "center")
  )
  
  
  
  
  if(position == "below" & message == "lovelace") return(bind_rows(logotext, lovelace_below))
  if(position == "right" & message == "lovelace") return(bind_rows(logotext, lovelace_right))
  if(position == "below" & message == "iwd") return(bind_rows(logotext, iwd_below))
  if(position == "right" & message == "iwd") return(bind_rows(logotext, iwd_right))
  return(logotext)
} 


