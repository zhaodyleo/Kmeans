#'kmeans_label_rematch
#'
#'match the true label based on the label existed
#'
#'@param Y_train label got from the kmeans_cluster from training data
#'@param Y_actual the actual label
#'@param Y_test predict label for testing data (optional)
#'@param Y_test_actual the actual test label
#'
#'@return  the vector of rematched label and training_accuracy.
#'    If Y_test_actual and Y_test are not null, return the testing accuracy as well.
#'
#'@examples
#'
#'
#'@import dplyr
#'@export
#

kmeans_label_rematch <- function(Y_train, Y_actual,Y_test = NULL,Y_test_actual = NULL){
  c_data = data.frame(Y_train, Y_actual)
  match_label = c_data %>%
    count(Y_actual,Y_train) %>%
    group_by(Y_actual) %>%
    filter(n == max(n)) %>%
    ungroup() %>% select(n)
}
