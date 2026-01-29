## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)

# Create a temporary file for this example
file <- tempfile(fileext = ".h5")

# Open the handle
h5 <- h5_open(file)

# The print method shows the file path and current internal working directory
print(h5)

## ----write_read---------------------------------------------------------------
# Write data using the handle
h5$write(1:10, "dataset1")
h5$write(matrix(1:9, 3, 3), "matrix_data")

# List contents
h5$ls()

# Read data back
my_data <- h5$read("dataset1")
print(my_data)

## ----navigation---------------------------------------------------------------
# Create a group structure
h5$create_group("simulations")

# Navigate into the group
h5$cd("simulations")
h5$pwd()

# Write data using relative paths (writes to /simulations/run1)
h5$write(rnorm(10), "run1")

# Verify the location
h5$ls()

## ----absolute_paths-----------------------------------------------------------
# Writes to the root, ignoring the fact that we are in /simulations
h5$write(100, "/root_dataset")

## ----relative_nav-------------------------------------------------------------
h5$cd("..")
h5$pwd() # Now back at "/"

## ----ref_semantics------------------------------------------------------------
h5_alias <- h5

# Change directory in the alias
h5_alias$cd("simulations")

# The original handle is also updated
h5$pwd()

## ----close--------------------------------------------------------------------
h5$close()

# Further attempts to use the handle will result in an error:
# h5$ls() 
# Error: This h5 file handle has been closed.

