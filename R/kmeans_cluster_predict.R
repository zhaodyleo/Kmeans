#'kmeans_cluster_predict
#'
#'predict the label for each new data it belongs
#'
#'@param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
#'@param center the centres for each clusters
#'
#'@return  label for each row observations
#'
#'@examples
#'
#'# One dimension example
#'  x = c(1,2,4,6,3)
#'  mu = c(2,4)
#'  kmeans_cluster_predict(x, mu)
#'# Multi dimension example
#'  X1 <- runif(1000,0,1)
#'  X2 <- runif(1000,0,1)
#'  X3 <- runif(1000,2,3)
#'  X4 <- runif(1000,2,3)
#'  train_data <- cbind(c(X1,X3),c(X2,X4))
#'  # Note the center for each cluster is columnwise
#'  # ensure the number of row of center equal to the number of column of X
#'  kmeans_cluster_predict(train_data , matrix(c(2,2,0,0),2,2))
#'@export
#

kmeans_cluster_predict <- function(X, center){
  if(is.null(dim(X))){
    SumOfSquare_diff <- sapply(center,
                               FUN = function(i) (X-i)^2)
    Cluster_index <- apply(SumOfSquare_diff,
                           MARGIN = 1,
                           FUN = function(i) which.min(i))
    return(Cluster_index)
  }else{
    X <- data.matrix(X)
    SumOfSquare_diff <- apply(center,
                              MARGIN = 2,
                              FUN = function(i) rowSums((sweep(X,MARGIN = 2,FUN = "-",i))^2))
    Cluster_index <- apply(SumOfSquare_diff,
                           MARGIN = 1,
                           FUN = function(i) which.min(i))
    return(Cluster_index)
  }
}
