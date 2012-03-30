freqDist <- function(x, bin, int = bin, relative=FALSE)
{
  require(ggplot2)
  
  x <- x[!is.na(x)]
  dy <- floor(x / bin)
  mn <- min(dy)
  mx <- max(dy)
  dy <- dy - mn + 1
  df <- data.frame(table(factor(dy, levels = 1:(mx - mn + 1))))
  names(df) <- c("range", "freq")
  df$range <- mn:mx * bin
  pcnt = df$freq / sum(df$freq) * 100
  cum.pcnt = cumsum(pcnt)
  ret <- data.frame(df, pcnt, cum = cum.pcnt)


  start <- df$range[1]
  end <- start + length(df$range) * bin

  pdata <- data.frame(x)

  if (relative) {
    g <- ggplot(pdata, aes(x = x, y = ..count.. / sum(..count..)))
  } else {
    g <- ggplot(pdata, aes(x))
  }
  g <- g + geom_histogram(binwidth = bin, fill = "#8080B0")
  g <- g + scale_x_continuous(breaks = seq(start, end, by = int), expand = c(0, 0))
  g <- g + labs(x="", y="")
  print(g)
  
  return(ret)
}
