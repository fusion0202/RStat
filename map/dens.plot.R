dens.plot <- function(shape, df, palette, title = "Population Density"){
  map <- ggplot(df)
  map <- map + aes(long,lat, group = group)
  map <- map + geom_polygon(aes(fill = id))
  map <- map + scale_fill_manual(values=attr(palette, "palette"), name="",
                               labels=names(attr(palette, "table")))
  map <- map + geom_path(data = shape,
                       aes(long,lat, group = group),
                       colour = "#a0a0a0")
  map <- map + xlim(123, 150) + 
    opts(title = title) +
    labs(x="", y="") + coord_equal() + 
    opts(axis.ticks = theme_blank(), 
          axis.text.x =  theme_blank(),
          axis.text.y =  theme_blank()) 
  map <- map + opts(legend.position = c(0.95, 0.40),
                    legend.justification = c(1, 1),
                    legend.background = theme_rect(fill="white", colour="white"),
                    legend.key=theme_rect(fill="white", colour="white"))
  print(map)
}