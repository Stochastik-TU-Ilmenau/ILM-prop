suppressPackageStartupMessages({
    library(lubridate)
})

min_date <- ymd("2021-05-01")
max_date <- today()

n_weeks <- 12

delays_to_nowcast <- 0:28
