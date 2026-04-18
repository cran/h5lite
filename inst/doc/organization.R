## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

## -----------------------------------------------------------------------------
# Create a deep hierarchy
h5_create_group(file, "project_A/simulation/run_01")

# Verify
h5_str(file)

## -----------------------------------------------------------------------------
# Define a complex structure in R
experiment_data <- list(
  metadata = list(
    id         = I(101),
    technician = I("Dr. Smith"),
    timestamp  = I("2023-10-27")
  ),
  measurements = list(
    raw         = runif(10),
    calibration = c(0.1, 0.9)
  ),
  status = I("complete")
)

# Write the entire structure to a group named "exp_101"
h5_write(experiment_data, file, "exp_101")

## -----------------------------------------------------------------------------
# List all objects recursively
h5_ls(file)

# Visualize the tree
h5_str(file)

## -----------------------------------------------------------------------------
# Rename 'exp_101' to 'archive_101'
h5_move(file, "exp_101", "archive_101")

# Move 'project_A' inside 'archive_101'
h5_move(file, "project_A", "archive_101/project_A")

h5_ls(file)

## -----------------------------------------------------------------------------
# Delete the entire archive group
h5_delete(file, "archive_101")

# The file is now empty (except for the root)
h5_ls(file)

## ----include=FALSE------------------------------------------------------------
unlink(file)

