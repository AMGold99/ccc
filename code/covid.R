library(tidyverse)
library(tidyr)
library(readr)

covid <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# I'm censuring a "decrease" in cumulative cases to zero; see https://github.com/nytimes/covid-19-data/issues/80 for more

covid_complete <- covid |>
  arrange(fips,date) |>
  mutate(fips_lag = lag(fips),
         cases_lag = case_when(
           fips == fips_lag ~ lag(cases)
           ),
         new_cases = case_when(
           (cases - cases_lag) >= 0 ~ cases - cases_lag,
           TRUE ~ 0
         )
  )

