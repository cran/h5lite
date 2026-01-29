## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

## -----------------------------------------------------------------------------
# Write a numeric vector
vec <- c(1.5, 2.3, 4.2, 5.1)
h5_write(vec, file, "data/numeric_vector")

# Read it back
res <- h5_read(file, "data/numeric_vector")
print(res)

## -----------------------------------------------------------------------------
# 1. Default: 1D Array (Length 1)
h5_write(42, file, "structure/array_1d")

# 2. Explicit Scalar: Wrapped in I()
h5_write(I(42), file, "structure/scalar")

h5_str(file, "structure")

## -----------------------------------------------------------------------------
# Integer vector with NO missing values -> Automatic optimal type (uint8)
h5_write(c(1L, 2L, 3L), file, "safe/ints")
h5_typeof(file, "safe/ints")

# Integer vector WITH missing values -> Promoted to float64
h5_write(c(1L, NA, 3L), file, "safe/ints_na")
h5_typeof(file, "safe/ints_na")

## -----------------------------------------------------------------------------
# Store small integers as 8-bit signed integers
h5_write(c(10, -5, 100), file, "small_ints", as = "int8")

# Store logicals as 8-bit unsigned integers
h5_write(c(TRUE, FALSE), file, "bools", as = "uint8")

## -----------------------------------------------------------------------------
# Variable length strings (handles NA)
h5_write(c("apple", "banana", NA), file, "strings/var")

## -----------------------------------------------------------------------------
# Fixed length strings (10 bytes per string)
h5_write(c("A", "B", "C"), file, "strings/fixed", as = "ascii[10]")

# Auto-detect max length (converts to fixed length based on longest string)
h5_write(c("short", "longer", "longest"), file, "strings/auto_fixed", as = "ascii[]")

## -----------------------------------------------------------------------------
# Write a large vector with compression
x <- rep(rnorm(100), 100)
h5_write(x, file, "compressed_data", compress = TRUE)

## -----------------------------------------------------------------------------
if (requireNamespace("bit64", quietly = TRUE)) {
  val <- bit64::as.integer64(c("9223372036854775807", "-9223372036854775807"))
  
  h5_write(val, file, "huge_ints")
  
  in_val <- h5_read(file, "huge_ints")
  print(class(in_val))
}

## ----include=FALSE------------------------------------------------------------
unlink(file)

