
source(here::here("source/core-functions.R"))


make_bare_logo <- function(colour = "black", background = "white") {
  
  # create the components
  logo <- generate_logo() %>% 
    mutate(x = x + .2)
  
  # specify plot limits
  x_limit <- c(-1, 2) # w: 3
  y_limit <- c(-1, 2) # h: 4
  
  # construct plot
  pic <- ggplot() +
    
    geom_polygon(
      data = logo,
      mapping = aes(x, y, group = object, subgroup = part),
      fill = voltron_palette[colour],
      colour = voltron_palette[colour],
      size = .5
    ) +
    
    coord_equal() +
    scale_size_identity() +
    theme_void() +
    theme(panel.background = element_rect(
      fill = voltron_palette[background], 
      colour = voltron_palette[background]
    )) + 
    scale_x_continuous(limits = x_limit, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limit, expand = c(0, 0)) +
    NULL
  
  # invisibly return the ggplot object: note that this object won't
  # render the way you want it to unless you export it in the exact 
  # width, height and dpi settings per export_logo()
  return(invisible(pic))
}



for(r in 1:nrow(visual_themes)) {
  
  col <- visual_themes$colour[r]
  bg <- visual_themes$background[r]
  
  dir <- here::here("img/bare-logo")
  fname <- paste0("bare-logo_", col, "-on-", bg, ".png")
  cat(fname, "\n")
  
  pic <- make_bare_logo(colour = col, background = bg)
  
  export_logo(
    plot = pic,
    path = file.path(dir, fname), 
    background = voltron_palette[bg], 
    height = 12,
    width = 12
  )
  
}

