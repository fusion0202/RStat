gini.coef <- function(x)
{
  if(length(which(is.na(x))) != 0){
    x <- x[-which(is.na(x))]
  }
  n <- length(x)
  x <- sort(x)
  x <- cumsum(x)
  x <- c(0, x/x[n])
  y <- seq(0, 1, length=n+1)
  return(2*sum(y-x)/n)
}


pop <- read.csv('pop1920-2010.csv', sep='\t', header=T)
area <- read.csv('area47.csv', sep=',', header=F)
pop <- pop[-1]
area <- area[-1]
area <- rep(area, times=8)
pop <- pop/area
res <- apply(pop, 2, gini.coef)
year <- c(1920, 1935, 1945, 1965, 1970, 1995, 2005, 2010)
plot(year, res, ylim=c(0.3,0.7), ylab='Gini Coefficient', las=1, type='b')
