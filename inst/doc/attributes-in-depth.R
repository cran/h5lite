## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

## -----------------------------------------------------------------------------
# First, write a dataset
h5_write(1:10, file, "measurements/temperature")

# Now, attach attributes to it
h5_write(I("Celsius"),    file, "measurements/temperature", attr = "units")
h5_write(I("2023-10-27"), file, "measurements/temperature", attr = "date")
h5_write(I(0.1),          file, "measurements/temperature", attr = "precision")

## -----------------------------------------------------------------------------
# Create a vector with custom R attributes
data <- rnorm(5)
attr(data, "description") <- I("Randomized control group")
attr(data, "valid")       <- I(TRUE)

# Write the object
h5_write(data, file, "experiment/control")

# Check the file - the attributes are there
h5_attr_names(file, "experiment/control")

h5_str(file)

## -----------------------------------------------------------------------------
# Read just the 'units' attribute
units <- h5_read(file, "measurements/temperature", attr = "units")
print(units)

## -----------------------------------------------------------------------------
# Read the full dataset
temps <- h5_read(file, "measurements/temperature")

# The attributes are available in R
attributes(temps)

str(temps)

## -----------------------------------------------------------------------------
h5_attr_names(file, "measurements/temperature")

## -----------------------------------------------------------------------------
# Delete the 'precision' attribute
h5_delete(file, "measurements/temperature", attr = "precision")

# Verify removal
h5_attr_names(file, "measurements/temperature")

## -----------------------------------------------------------------------------
# A vector with names
named_vec <- c(a = 1, b = 2, c = 3)

# Write as a standard Dataset -> Names are preserved
h5_write(named_vec, file, "my_dataset")
h5_names(file, "my_dataset")

# Write as an Attribute -> Names are LOST
h5_write(named_vec, file, "measurements/temperature", attr = "meta_vec")
h5_names(file, "measurements/temperature", attr = "meta_vec")

## -----------------------------------------------------------------------------
# A data frame with metadata
df <- data.frame(
  id = 1:3, 
  status = c("ok", "fail", "ok")
)

# Write as attribute
h5_write(df, file, "measurements/temperature", attr = "log")

# Column names survive!
h5_names(file, "measurements/temperature", attr = "log")

## -----------------------------------------------------------------------------
# Write the temperature data again, but use a fixed length string for 'description'
h5_write(data, file, "experiment/control", as = c("@description" = "ascii[]"))

# Store an attribute as a `uint8` instead of the default `int32`
h5_write(I(42), file, "measurements/temperature", "sensor_id", as = "uint8")

## -----------------------------------------------------------------------------
# Force the 'valid' attribute to be read as logical, even if stored as integer
meta <- h5_read(file, "experiment/control", attr = "valid", as = "logical")

## ----include=FALSE------------------------------------------------------------
unlink(file)

