https.csv <- function(url, header=F){  
  text1 <- getURL(url, ssl.verifypeer = FALSE)
  text2 <- unlist(strsplit(text1, "\n"))
  head1 <- unlist(strsplit(text2[1], split="[[:blank:]]"))
  head2 <- unlist(strsplit(head1, ","))
  n.col <- length(head2)
  text3 <- unlist(strsplit(text2, split="[[:blank:]]"))  
  text4 <- unlist(strsplit(text3, ","))
  text5 <- text4[text4 != ""]
  if(header){
    data <- data.frame(matrix(as.numeric(text5[-(1:n.col)]), ncol=n.col, byrow=T))
    names(data) <- head2
  } else {
    data <- data.frame(matrix(as.numeric(text5), ncol=n.col, byrow=T))
  }  
  return(data)
}

