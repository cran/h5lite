## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)

## -----------------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

temps <- round(20 + cumsum(rnorm(1e5, sd = 0.05)), 2)
h5_write(temps, file, "temps/default")

h5_inspect(file, "temps/default")

## -----------------------------------------------------------------------------
h5_write(temps, file, "temps/zstd", compress = "zstd-5")
h5_write(temps, file, "temps/none", compress = "none")

## -----------------------------------------------------------------------------
cmp <- h5_compression("zstd-5", checksum = TRUE, chunk_size = 256 * 1024)
cmp

h5_write(temps, file, "temps/configured", compress = cmp)

## -----------------------------------------------------------------------------
words <- sample(c("control", "treatment", "placebo"), 1e4, replace = TRUE)
h5_write(words, file, "strings/fixed")
h5_inspect(file, "strings/fixed")

## -----------------------------------------------------------------------------
compression_ratio <- function(x, codecs, ...) {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))
  sapply(codecs, function(codec) {
    h5_write(x, tmp, "x", compress = h5_compression(codec, ...))
    info <- h5_inspect(tmp, "x")
    round(info$uncompressed_size / info$storage_size, 2)
  })
}

codecs <- c("gzip", "zstd-5", "zstd-19", "lz4", "bshuf-lz4",
            "bshuf-zstd-5", "blosc2-lz4-5", "blosc2-zstd-5")

# A slowly varying sensor signal (float64)
compression_ratio(temps, codecs)

# Small counts (stored as uint8)
compression_ratio(rpois(1e5, lambda = 3), codecs)

## -----------------------------------------------------------------------------
h5_write(temps, file, "lossy/zfp", compress = "zfp-acc-0.01")

# No value has moved by more than 0.01
max(abs(h5_read(file, "lossy/zfp") - temps))

h5_inspect(file, "lossy/zfp")

## -----------------------------------------------------------------------------
# Stored as uint16, so ZFP is skipped and gzip is used instead
h5_write(1:1000, file, "lossy/zfp_skipped", compress = "zfp-rev")
h5_inspect(file, "lossy/zfp_skipped")

# Storing as int32 lets ZFP run
h5_write(1:1000, file, "lossy/zfp_int32", compress = "zfp-rev", as = "int32")
h5_inspect(file, "lossy/zfp_int32")

## -----------------------------------------------------------------------------
# Integers in a narrow range: lossless
ids <- sample(100000:100100, 1e5, replace = TRUE)
h5_write(ids, file, "packed/ids", compress = h5_compression("gzip", int_packing = TRUE))
identical(h5_read(file, "packed/ids"), ids)

# Floats kept to 2 decimal places: lossy
readings <- rnorm(1e5, mean = 98.6, sd = 0.5)
h5_write(readings, file, "packed/readings",
         compress = h5_compression("gzip", float_rounding = 2))
max(abs(h5_read(file, "packed/readings") - readings))

h5_inspect(file, "packed/readings")

## -----------------------------------------------------------------------------
x <- rnorm(1e5)
h5_write(x, file, "lossy/truncated",
         compress = h5_compression("blosc2-zstd-5", blosc2_truncate = 16))

# Largest relative error
max(abs((h5_read(file, "lossy/truncated") - x) / x))

h5_inspect(file, "lossy/truncated")

## -----------------------------------------------------------------------------
blosc2_codecs <- c("blosc2-zstd-5", "blosc2-lz4-5")
rbind(
  without_delta = compression_ratio(temps, blosc2_codecs),
  with_delta    = compression_ratio(temps, blosc2_codecs, blosc2_delta = TRUE) )

## -----------------------------------------------------------------------------
mat <- matrix(rnorm(1e6), nrow = 10000, ncol = 100)   # 8 MB of float64

h5_write(mat, file, "chunks/default")
h5_inspect(file, "chunks/default")

h5_write(mat, file, "chunks/small", compress = h5_compression(chunk_size = 64 * 1024))
h5_inspect(file, "chunks/small")

## -----------------------------------------------------------------------------
h5_inspect(file, "temps/configured")

## -----------------------------------------------------------------------------
info <- h5_inspect(file, "temps/configured")
info$storage_size
sapply(info$filters, `[[`, "name")

## ----include = FALSE----------------------------------------------------------
unlink(file)

