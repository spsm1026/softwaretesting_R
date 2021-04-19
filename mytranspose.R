mytranspose <- function(x){
  # if input data is NULL.....
  if(identical(x,NULL)){
    return(NULL)
  }
  # if the input x is not matrix.....
  if(identical(nrow(x),NULL)){
    x <- matrix(x,nrow=1,ncol=length(x))
  }
  # if the matrix has 0 row and 0 col
  if (nrow(x)==0 & ncol(x)==0){
    return(matrix(NA, nrow=0, ncol=0))
  }
  if(is.data.frame(x)){
    result <- data.frame(t(x))
    colnames(result) <- rownames(x)
    rownames(result) <- colnames(x)
    return(result)
  }
  y <- matrix(1, nrow = ncol(x), ncol = nrow(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      y[j,i] <- x[i,j]
    }
  }
  return(y)
}