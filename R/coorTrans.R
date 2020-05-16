coorTrans <- function(b){
  library("impute")
  # least squares solutions for Ax=b
  U4529d3.m  <- cbind(cbind(U4529d3_1.m, U4529d3_2.m), U4529d3_3.m)
  A <- U4529d3.m %*% S450.m;

  tmp.idx <- match(rownames(A), rownames(b))
  b <- b[tmp.idx,]
  if(length(which(is.na(b))) > 0){
    b <- impute.knn(b , k = 10)$data
  }

  result <- solve(crossprod(A), crossprod(A, b));
  return(result);
}


