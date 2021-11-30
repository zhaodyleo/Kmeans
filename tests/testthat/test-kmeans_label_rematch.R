X1 = c(2,1,3,2,3,3,1)
X2 = c(1,3,2,1,2,2,3)
X3 = c(3,2,1,3,2,1,2,3,2)
X4 = c(2,1,3,2,1,3,1,2,1)


test_that("accuracy check", {
  expect_equal(kmeans_label_rematch(X1,X2)[[2]],1)
  expect_equal(kmeans_label_rematch(X1,X2,X3)[[2]],X4)
  expect_equal(kmeans_label_rematch(X1,X2,X3,X4)[[4]],1)
})

