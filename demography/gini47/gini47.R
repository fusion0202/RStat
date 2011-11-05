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


pop <- read.csv('pop1920-2010.csv', sep=',', header=F)
pop <- pop[-1]
year <- unlist(head(pop, 1))
pop <- tail(pop, 47)
area <- read.csv('area47.csv', sep=',', header=F)
area <- area[-1]
area <- rep(area, times=length(year))
pop <- pop/area
res <- apply(pop, 2, gini.coef)
par(las=1, mgp=c(2.5,0.8,0))
plot(year, res,  ylim=c(0.3,0.7), ylab='Gini Coefficient',type='n')
abline(v=1945, col='gray')
points(year,res, pch=19, col='red')
lines(year, res, col='red')