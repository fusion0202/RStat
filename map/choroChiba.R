choroChiba <- function(shape, rank, legend, title=""){
  require(ggplot2)
  rank <- rank - 1
  df <- fortify(spCbind(shape, rank), region = "rank")
  
  for(i in 0:max(rank)){
    if(sum(rank == i) == 0 ){
      dummy <- df[1,]
      dummy$id <- i
      df <- rbind(df, dummy)
    }
  }
  
  map <- ggplot(df)
  map <- map + aes(long,lat, group = group)
  map <- map + geom_polygon(aes(fill = id))
  map <- map + scale_fill_manual(values = legend[[1]], 
                                 labels = legend[[2]])
  map <- map + geom_path(data = shape,
                         aes(long,lat, group = group),
                         colour = "#303030", size = I(0.10))
  map <- map + xlim(139.7, 140.9) + 
    opts(title = title) +
    labs(x = "", y = "") + coord_equal() + 
    opts(axis.ticks = theme_blank(), 
         axis.text.x =  theme_blank(),
         axis.text.y =  theme_blank()) 
  map <- map + opts(legend.position = c(0.99, 0.01),
                    legend.justification = c("right", "bottom"),
                    legend.background = theme_rect(fill = "white", colour = "white"),
                    legend.key=theme_rect(fill = "white", colour = "white"),
                    legend.title=theme_blank())
  print(map)
}
