<!-- badges: start -->
  [![R-CMD-check](https://github.com/zhaodyleo/Kmeans/workflows/R-CMD-check/badge.svg)](https://github.com/zhaodyleo/Kmeans/actions)
  [![Coverage Status](https://coveralls.io/repos/github/zhaodyleo/Kmeans/badge.svg?branch=main)](https://coveralls.io/github/zhaodyleo/Kmeans?branch=main)
  <!-- badges: end -->

# Kmeans

--------------------------------

k-means clustering is a method of vector quantization that aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean (cluster centers or cluster centroid)

## How to install the library 

To install the development version of Kmeans from GitHub using devtools:

```
library(devtools)
install_github("zhaodyleo/Kmeans")
```

## Vignettes and examples
To build the vignettes for Celda and DecontX during installation from GitHub, use the following command:
```
library(devtools)
install_github("zhaodyleo/Kmeans", build_vignettes = TRUE)
```

The Kmeans vignettes can then be accessed via the following commands:
```
vignette("kmeans_Leo")
```
