freqDist <- function(x, w, brk = w, percent=FALSE)
{
  require(ggplot2)

  x <- x[!is.na(x)]
  y <- floor(x/w)
  mn <- min(y)
  mx <- max(y)
  y <- y - mn + 1
  dt <- data.frame(table(factor(y, levels=1:(mx-mn+1))))
  names(dt) <- c("x", "freq")
  dt$x <- mn : mx * w
  pcnt = dt$freq/sum(dt$freq)*100
  cum.pcnt = cumsum(pcnt)
  mid =dt$x + w/2
  pdata <- data.frame(dt, pcnt, cum.pcnt, mid)
  start <- dt$x[1]
  end <- start + length(dt$x)*w
  if(percent){
    g <- ggplot(pdata, aes(mid, pcnt))
  } else {
    g <- ggplot(pdata, aes(mid, freq)) 
  }
  g <- g + geom_bar(stat = "identity", fill="#4040FF")
  g <- g + scale_x_continuous(breaks = seq(start, end, by=brk))
  g <- g + labs(x="", y="")
  print(g)

  return(pdata[,colnames(pdata) != "mid"])
}
 
