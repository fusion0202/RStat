function(shape, rank, legend, title="", hmargin = 0, vmargin = 0){
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
  map <- map + xlim(shape@bbox[1], shape@bbox[3] + hmargin) +
    ylim(shape@bbox[2] - vmargin, shape@bbox[4]) +
    labs(title = title) +
    labs(x = "", y = "") + coord_equal() + 
    theme(axis.ticks = element_blank(), 
         axis.text.x =  element_blank(),
         axis.text.y =  element_blank()) 
  map <- map + theme(legend.position = c(0.99, 0.01),
                    legend.justification = c("right", "bottom"),
                    legend.background = element_rect(fill = "white", colour = "white"),
                    legend.key=element_rect(fill = "white", colour = "white"),
                    legend.title=element_blank())
  print(map)
}
