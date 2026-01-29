## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

# 1. 1D Array (Vector)
vec <- c(1.5, 2.3, 4.2)
h5_write(vec, file, "examples/vector")

# 2. 2D Array (Matrix)
mat <- matrix(1:9, nrow = 3, ncol = 3)
h5_write(mat, file, "examples/matrix")

# 3. 3D Array
arr <- array(1:24, dim = c(4, 3, 2))
h5_write(arr, file, "examples/array_3d")

# 4. Scalar
# By default, R treats length-1 vectors as arrays. 
# Wrap in I() to write a true HDF5 scalar.
val <- I(42)
h5_write(val, file, "examples/scalar")

## -----------------------------------------------------------------------------
x <- h5_read(file, "examples/matrix")
print(x)

## -----------------------------------------------------------------------------
my_list <- list(
  config = list(id = 1L, status = "active"),
  data   = runif(10)
)
# Creates a group "/experiment" containing "config" (group) and "data" (dataset)
h5_write(my_list, file, "experiment") 

## -----------------------------------------------------------------------------
df <- data.frame(
  id = 1:5,
  val = c(1.1, 2.2, 3.3, 4.4, 5.5)
)
h5_write(df, file, "study_data")

## -----------------------------------------------------------------------------
# Write a dataset
h5_write(1:10, file, "measurements")

# Attach an attribute to it
h5_write("meters", file, "measurements", attr = "units")

## -----------------------------------------------------------------------------
# List contents
h5_ls(file)

# Print structure tree (like R's str())
h5_str(file)

