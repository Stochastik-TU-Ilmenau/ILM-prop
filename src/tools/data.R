suppressPackageStartupMessages({
    library(readr)
    library(stringr)
    library(here)
    
    options(readr.show_col_types = FALSE, readr.show_progress = FALSE)
})

root_dir <- here()
data_dir <- "data"

read_data <- function(type) function(file, ...) read_csv(file.path(root_dir, data_dir, type, file), ..., col_types = cols(), progress = F)

read_raw <- read_data("raw")
read_external <- read_data("external")
read_interim <- read_data("interim")
read_processed <- read_data("processed")

write_data <- function(type) function(x, file, ...) write_csv(x, file.path(root_dir, data_dir, type, file), ...)

write_raw <- write_data("raw")
write_external <- write_data("external")
write_interim <- write_data("interim")
write_processed <- write_data("processed")

# helper to read data from our computing server
# if we are already on it (ssh session), use the file there
read_maurice <- function(fname, ...) {
    if (!file.exists(fname)) {
        fname <- pipe(paste0("ssh maurice 'cat ", fname, "'"))
    }
    read_csv(fname, ...)
}