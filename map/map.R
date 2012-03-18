map <- function(code.list,
                density=NULL,
                color=NULL)
{
  map0 <- function(data, dens, color)
  {
    continue <- apply(data, 1, any)
    plot(lon, lat, type = "n", axes=FALSE,
         xlab="", ylab="", bty="n", asp=1)
    start <- 1
    k <- 0
    for (i in 2:nrow(data)) {
      if (continue[i] == FALSE) {
        k <- k+1
        if (i-start == 4) {
          lines(data[start:(i-1),])
        }
        else {
          polygon(data[start:(i-1),], density=dens[k], col=color[k], border="black")
        }
        start <- i+1
      }
    }
  }
  
  
  
  for (i in seq(along=code.list)) {
    if (code.list[i] %in% c(15, 28, 47)) {
      code.list <- c(code.list, -code.list[i])
      density <- c(density, density[i])
      color <- c(color, color[i])
    }
  }
  code.list[code.list == -15] <- 48
  code.list[code.list == -28] <- 49
  code.list[code.list == -47] <- 50
  
  lon <- lat <- NULL
  for (i in code.list) {
    fn <- sprintf("jpn/%02i", i)
    gwm <- matrix(scan(fn, quiet=TRUE), ncol=2, byrow=TRUE)
    lon <- c(lon, gwm[,1], 0)
    lat <- c(lat, gwm[,2], 0)
  }
  mlon <- min(lon[lon != 0])
  mlat <- max(lat[lat != 0])
  lon <- ifelse(lon == 0, 0, lon-mlon+1)
  lat <- ifelse(lat == 0, 0, mlat-lat+1)
  map0(cbind(as.integer(lon), as.integer(lat)), density, color)
}
