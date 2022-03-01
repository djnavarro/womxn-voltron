
source(here::here("source/core-functions.R"))


make_banner_logo <- function(colour = "black", background = "white") {
  
  # create the components
  voltron_text <- generate_logotype() 
  logo <- generate_logo() %>% 
    mutate(x = x + .2)
  
  # specify plot limits
  x_limit <- c(-4, 12) # w: 16
  y_limit <- c(-1.5, 2.5) # h: 4
  
  # construct plot
  pic <- ggplot() +
    
    geom_polygon(
      data = logo,
      mapping = aes(x, y, group = object, subgroup = part),
      fill = voltron_palette[colour],
      colour = voltron_palette[colour],
      size = .5
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
      colour = voltron_palette[colour]
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
  
  dir <- here::here("img/banner-logo")
  fname <- paste0("banner-logo_", col, "-on-", bg, ".png")
  cat(fname, "\n")
  
  pic <- make_banner_logo(colour = col, background = bg)
  
  export_logo(
    plot = pic,
    path = file.path(dir, fname), 
    background = voltron_palette[bg], 
    height = 6,
    width = 24
  )
  
}

