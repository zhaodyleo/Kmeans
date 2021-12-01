#'kmeans_label_rematch
#'
#'match the true label based on the label existed
#'
#'@param Y_train label got from the kmeans_cluster from training data
#'@param Y_actual the actual label
#'@param Y_test predict label for testing data (optional)
#'@param Y_test_actual the actual test label(optional)
#'
#'@return  the vector of rematched label and training_accuracy.
#'    If Y_test_actual and Y_test are not null, return the testing accuracy as well.
#'
#'@examples
#' X1 = c(1,3,2,2,1,2,2)
#' X2 = c(2,5,1,1,2,1,1)
#' kmeans_label_rematch(X1,X2)
#'
#'@import dplyr
#'@export
#

kmeans_label_rematch <- function(Y_train, Y_actual,Y_test = NULL,Y_test_actual = NULL){
  Y_train_rematch <- Y_train
  Y_test_rematch <- Y_test
  c_data <- data.frame(Y_train = Y_train, Y_actual = Y_actual)
  match_label <- c_data %>%
    count(Y_actual,Y_train) %>%
    group_by(Y_actual) %>%
    filter(n == max(n)) %>%
    ungroup()
  for(i in 1:nrow(match_label)){
    Y_train_rematch[Y_train == as.numeric(match_label[i,2])] <- match_label[i,1]
    if(!is.null(Y_test)){
      Y_test_rematch[Y_test == as.numeric(match_label[i,2])] <- match_label[i,1]
    }
  }
  Y_train_rematch <- unlist(Y_train_rematch)
  Y_test_rematch <- unlist(Y_test_rematch)
  if(is.null(Y_test)){
    return(list(label  <- Y_train_rematch,
                accuracy  <- mean(Y_train_rematch == Y_actual)))
  }else{
    if(is.null(Y_test_actual)){
      return(list(train_label  <- Y_train_rematch,
                  test_label  <- Y_test_rematch,
                  train_accuracy <- mean(Y_train_rematch == Y_actual) ))
    }else{
      return(list(train_label  <- Y_train_rematch,
                  test_label  <- Y_test_rematch,
                  train_accuracy <- mean(Y_train_rematch == Y_actual),
                  test_accuracy <- mean(Y_test_rematch == Y_test_actual)))
    }
  }

}
