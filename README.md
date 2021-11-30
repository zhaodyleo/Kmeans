<!-- badges: start -->
  [![R-CMD-check](https://github.com/zhaodyleo/Kmeans/workflows/R-CMD-check/badge.svg)](https://github.com/zhaodyleo/Kmeans/actions)
  [![codecov](https://codecov.io/gh/zhaodyleo/Kmeans/branch/main/graph/badge.svg?token=IX9YM93CAO)](https://codecov.io/gh/zhaodyleo/Kmeans)
  <!-- badges: end -->

# Kmeans

--------------------------------

k-means clustering is a method of vector quantization that aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean (cluster centers or cluster centroid)

There are three functions include in this library which are 

- kmeans_cluster: training the model to determine the center for each cluster and the cluster each observations it belongs.
- kmeans_cluster_predict: using the center to determine the cluster new data belongs
- kmeans_label_rematch: match the trained cluster label with the actual cluster label.

Check the detail information for each functions through help page or vignettes.

## How to install the library 

To install the development version of Kmeans from GitHub using devtools:

```
library(devtools)
install_github("zhaodyleo/Kmeans")
```

## Vignettes and examples

To build the vignettes for kmeans during installation from GitHub, use the following command:
```
library(devtools)
install_github("zhaodyleo/Kmeans", build_vignettes = TRUE)
```

The Kmeans vignettes can then be accessed via the following commands:
```
vignette("kmeans_Leo")
```
