library(readr)
library(usethis)

daily_crumb_headers <- read_csv("data-raw/daily_crumb_headers.csv")
usethis::use_data(daily_crumb_headers, overwrite = TRUE)
