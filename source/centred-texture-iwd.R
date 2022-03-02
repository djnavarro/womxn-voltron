
source(here::here("source/core-functions.R"))
source(here::here("source/texture-functions.R"))

make_centred_texture_iwd <- function(colour, background, seed = 1000) {

  # fix the seed 
  set.seed(seed)
  
  # specify plot limits 
  x_limit <- c(-4, 12) # w: 16
  y_limit <- c(-3.5, 4.5) # h: 8
  
  # define the texture
  blank_fn <- function(x, y) y < 2.5 & y > -1.5
  texture <- make_texture(x_limit, y_limit, blank_fn)
  
  # move foreground up
  ysh <- .5
  xsh <- 0
  
  # define the logo & text
  voltron_text <- generate_logotype(position = "below", message = "iwd") 
  logo <- generate_logo() %>% mutate(x = x + .6)
  
  # construct plot
  pic <- ggplot() +
    
    # texture-trapezoids
    geom_polygon(
      data = texture$trapezoids,
      mapping = aes(x, y, group = id, subgroup = part, alpha = alpha),
      fill = voltron_palette[colour],
      colour = voltron_palette[colour],
      size = .1, 
      show.legend = FALSE
    ) +
    
    # texture-brackets
    geom_polygon(
      data = texture$brackets,
      mapping = aes(x, y, group = id, alpha = alpha),
      fill = voltron_palette[colour],
      colour = voltron_palette[colour],
      size = .1, 
      show.legend = FALSE
    ) +
    
    # foreground-logo
    geom_polygon(
      data = logo,
      mapping = aes(x + xsh, y + ysh, group = object, subgroup = part),
      fill = voltron_palette[colour],
      colour = voltron_palette[colour],
      size = .5
    ) +
    
    # foreground-text
    geom_text(
      data = voltron_text,
      mapping = aes(
        x + xsh, y + ysh,
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
    scale_alpha_identity() + 
    theme_void() +
    theme(panel.background = element_rect(
      fill = voltron_palette[background], 
      colour = voltron_palette[background]
    )) + 
    scale_x_continuous(limits = x_limit, expand = c(0, 0), oob = scales::oob_keep) +
    scale_y_continuous(limits = y_limit, expand = c(0, 0), oob = scales::oob_keep) +
    NULL
  
  # invisibly return the ggplot object: note that this object won't
  # render the way you want it to unless you export it in the exact 
  # width, height and dpi settings per export_logo()
  return(invisible(pic))
}





for(r in 1:nrow(visual_themes)) {
  
  col <- visual_themes$colour[r]
  bg <- visual_themes$background[r]
  
  dir <- here::here("img/centre-texture-iwd")
  fname <- paste0("centre-texture-iwd_", col, "-on-", bg, ".png")
  cat(fname, "\n")
  
  pic <- make_centred_texture_iwd(colour = col, background = bg)
  
  export_logo(
    plot = pic,
    path = file.path(dir, fname), 
    background = voltron_palette[bg], 
    height = 12,
    width = 24
  )
  
}

