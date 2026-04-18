## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

## -----------------------------------------------------------------------------
# 1. Scalar (0 dims)
h5_write(I(42), file, "structure/scalar")

# 2. Vector (1 dim)
h5_write(c(1, 2, 3), file, "structure/vector")

# 3. Matrix (2 dims)
h5_write(matrix(1:9, 3, 3), file, "structure/matrix")

## -----------------------------------------------------------------------------
# Integers between 0 and 255 (uint8)
h5_write(c(1L, 2L, 3L), file, "integers/small")

# Integers with NA -> float64
h5_write(c(1L, NA, 3L), file, "integers/with_na")

# Force larger type (int16)
h5_write(1:100, file, "integers/short", as = "int16")

## -----------------------------------------------------------------------------
if (requireNamespace("bit64", quietly = TRUE)) {
  val <- bit64::as.integer64(c("9223372036854775807", "-9223372036854775807"))
  h5_write(val, file, "integers/int64")
}

## -----------------------------------------------------------------------------
data <- rnorm(10)

# Default (float64)
h5_write(data, file, "doubles/default")

# Single Precision (float32) - Saves 50% space
h5_write(data, file, "doubles/float32", as = "float32")

## -----------------------------------------------------------------------------
bools <- sample(c(TRUE, FALSE), 1000, replace = TRUE)

h5_write(bools, file, "logicals/packed")

## -----------------------------------------------------------------------------
# UTF-8 auto-detected fixed length
h5_write(c("apple", "banana"), file, "strings/fixed_utf8")

# ASCII fixed length (1 byte)
h5_write(c("A", "B", "C"), file, "strings/fixed_ascii", as = "ascii[1]")

## -----------------------------------------------------------------------------
now <- Sys.time()
h5_write(now, file, "datetime/iso8601")

## -----------------------------------------------------------------------------
comp <- c(1+2i, 3+4i)
h5_write(comp, file, "complex_data")

## -----------------------------------------------------------------------------
raw_vec <- as.raw(c(0x01, 0xFF, 0x1A))
h5_write(raw_vec, file, "binary_blob")

## -----------------------------------------------------------------------------
fac <- factor(c("low", "high", "medium", "low"))
h5_write(fac, file, "categorical")

## -----------------------------------------------------------------------------
my_list <- list(data = 1:100, meta = list(valid = TRUE))
h5_write(my_list, file, "types/list")

## -----------------------------------------------------------------------------
df <- data.frame(
  id = 1:5,
  score = c(10.5, 20.2, 15.0, 9.8, 30.1)
)

# 1. 'id' coerced to uint16
# 2. 'score' coerced to float32
h5_write(df, file, "types/dataframe", as = c(
  "id"    = "uint16",
  "score" = "float32"
))

## -----------------------------------------------------------------------------
h5_write(NULL, file, "placeholders/empty_slot")

## -----------------------------------------------------------------------------
# Maximum zlib compression
h5_write(rnorm(1000), file, "data/max", compress = "gzip-9")

# Szip Entropy Coding for discrete integer data
h5_write(sample(1:5, 1000, replace = TRUE), file, "data/szip", compress = "szip-ec")

## ----include = FALSE----------------------------------------------------------
unlink(file)

