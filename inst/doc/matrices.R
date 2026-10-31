## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

## -----------------------------------------------------------------------------
# Create a 3x4 matrix
mat <- matrix(1:12, nrow = 3, ncol = 4)

# Write to file
h5_write(mat, file, "linear_algebra/mat_a")

# Read back
mat_in <- h5_read(file, "linear_algebra/mat_a")

# Verify
all.equal(mat, mat_in)

## -----------------------------------------------------------------------------
# Create a 3D array (e.g., spatial data over time: x, y, time)
vol <- array(runif(24), dim = c(4, 3, 2))

h5_write(vol, file, "spatial/volume")

# Check dimensions without reading the full data
h5_dim(file, "spatial/volume")

## -----------------------------------------------------------------------------
# Create a matrix with row and column names
data <- matrix(rnorm(6), nrow = 2)
rownames(data) <- c("Sample_A", "Sample_B")
colnames(data) <- c("Gene_1", "Gene_2", "Gene_3")

h5_write(data, file, "genetics/expression")

# Read back
data_in <- h5_read(file, "genetics/expression")

print(data_in)

## -----------------------------------------------------------------------------
# A large, mostly-zero matrix (highly compressible)
sparse_mat <- matrix(0, nrow = 1000, ncol = 1000)
sparse_mat[1:10, 1:10] <- 1

# Default compression (gzip level 5)
h5_write(sparse_mat, file, "compressed/matrix")

# Zstandard: faster than gzip, with a similar or better ratio
h5_write(sparse_mat, file, "compressed/matrix_zstd", compress = "zstd-5")

h5_inspect(file, "compressed/matrix_zstd")

## -----------------------------------------------------------------------------
big <- matrix(1:50000, nrow = 10000, ncol = 5)
h5_write(big, file, "big_matrix")

# Read 3 rows, starting at row 5001
h5_read(file, "big_matrix", start = 5001, count = 3)

## ----include=FALSE------------------------------------------------------------
unlink(file)

