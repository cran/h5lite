## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(h5lite)
file <- tempfile(fileext = ".h5")

## -----------------------------------------------------------------------------
# Create a standard data frame
df <- data.frame(
  id = 1:5,
  group = c("A", "A", "B", "B", "C"),
  score = c(10.5, 9.2, 8.4, 7.1, 6.0),
  passed = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# Write to HDF5
h5_write(df, file, "study_data/results")

# Fetch the column names
h5_names(file, "study_data/results")

# Read back
df_in <- h5_read(file, "study_data/results")

head(df_in)

## -----------------------------------------------------------------------------
df_small <- data.frame(
  id   = 1:10,
  code = rep("A", 10)
)

# Force 'id' to be uint16 and 'code' to be an ascii string
h5_write(df_small, file, "custom_df", 
         as = c(id = "uint16", code = "ascii[]"))

## -----------------------------------------------------------------------------
mtcars_subset <- head(mtcars, 3)

h5_write(mtcars_subset, file, "cars")

h5_str(file)

# Read back
result <- h5_read(file, "cars")
print(row.names(result))

## ----include=FALSE------------------------------------------------------------
unlink(file)

