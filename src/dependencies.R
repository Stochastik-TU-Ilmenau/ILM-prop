# List of package dependencies
packages <- c(
    "here",
    "lubridate",
    "reshape2",
    "tidyverse",
    "testthat",
    "zoo"
)

# Function to check and install packages
check_and_install <- function(pkg){
  if(!(pkg %in% installed.packages()[,"Package"])) {
    install.packages(pkg)
  }
}

# Use lapply to check and install packages
lapply(packages, check_and_install)