color.map3 <- function( x,
                        t,
                        color.set)
{
  if (length(t)+1 != length(color.set)) {
    stop("t の長さは color.set の長さより 1 だけ小さくなければならない")
  }
  map(1:47, color=color.set[findInterval(x, t)+1])
}
