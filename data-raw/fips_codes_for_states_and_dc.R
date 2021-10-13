library(readr)
library(usethis)

fips_codes_for_states_and_dc <- read_csv("data-raw/fips_codes_for_states_and_dc.csv")
usethis::use_data(fips_codes_for_states_and_dc, overwrite = TRUE)
