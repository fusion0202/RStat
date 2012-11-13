RMeCabDF <- function (dataf, coln, mypref = 0, dic = "", mecabrc = "", etc = "") 
{
  if (!is.data.frame(dataf)) {
    stop("the first argument must be a data frame!")
  }
  kekka <- list(length(dataf[, coln]))
  for (i in 1:length(dataf[, coln])) {
    if (!is.factor(dataf[i, coln]) || is.na(dataf[i, coln]) || 
      dataf[i, coln] == "") {
      kekka[[i]] <- NA
    }
    else {
      kekka[[i]] <- unlist(RMeCabC(as.character(dataf[i, coln]), mypref, 
                                   dic, mecabrc, etc))
    }
  }
  return(kekka)
} 
