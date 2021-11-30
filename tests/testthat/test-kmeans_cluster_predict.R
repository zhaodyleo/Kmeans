X1 = c(2,3,12,11,2,1,9)
X2 = c(1,1,2,2,1,1,2)


test_that("accuracy check", {
  expect_equal(kmeans_cluster_predict(X1,c(4,8)), X2)
})
