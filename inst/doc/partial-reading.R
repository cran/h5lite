## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----single-value-------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

# --- 1. Vectors (Element-level targeting) ---
h5_write(seq(10, 100, by = 10), file, "my_vector")

# Start at the 4th element, read 3 elements
h5_read(file, "my_vector", start = 4, count = 3)

# --- 2. Matrices (Row-level targeting) ---
mat <- matrix(1:50, nrow = 10, ncol = 5)
h5_write(mat, file, "my_matrix")

# Start at row 5, read 3 complete rows (automatically spans all columns)
h5_read(file, "my_matrix", start = 5, count = 3)

# --- 3. Data Frames (Row-level targeting) ---
h5_write(mtcars, file, "my_mtcars")

# Start at row 10, read 5 complete rows
h5_read(file, "my_mtcars", start = 10, count = 5)

# --- 4. 3D Arrays (Matrix-level targeting) ---
arr <- array(1:24, dim = c(2, 3, 4)) 
h5_write(arr, file, "my_array")

# Start at the 2nd matrix, read 2 complete matrices
h5_read(file, "my_array", start = 2, count = 2)

## ----exact-index--------------------------------------------------------------
# Read exactly row 5 of the matrix. 
# The row dimension is dropped, returning a 1D vector.
row_vec <- h5_read(file, "my_matrix", start = 5)
row_vec

class(row_vec)

## ----range-index--------------------------------------------------------------
# Read row 5, but signal a range request by setting count = 1.
# The original geometry is preserved, returning a 1x5 matrix.
row_mat <- h5_read(file, "my_matrix", start = 5, count = 1)
row_mat

class(row_mat)

## ----multi-value--------------------------------------------------------------
# Matrix: Start at row 5, column 2, and read 3 elements along that row.
# The row is an exact point index (dropped). The columns are a range (preserved).
# Returns a 1D vector of length 3.
h5_read(file, "my_matrix", start = c(5, 2), count = 3)

# Matrix: Extract exactly row 5, column 2. 
# Because count is omitted, the final dimension is also dropped.
# Returns an unnamed scalar value.
h5_read(file, "my_matrix", start = c(5, 2))

# 3D Array: Target matrix 2, row 1.
# The matrix and row are exact point indices (dropped). 
# Returns a 1D vector containing the columns of that specific row.
h5_read(file, "my_array", start = c(2, 1))

## ----cleanup, include=FALSE---------------------------------------------------
unlink(file)

