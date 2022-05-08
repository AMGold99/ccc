library(tidyverse)
library(tidyr)
library(readr)

covid <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# I'm censuring a "decrease" in cumulative cases to zero; see https://github.com/nytimes/covid-19-data/issues/80 for more

actual_states <- statepop |> pull(full) |> unique()

nyt_states <- covid |> pull(state) |> unique()

nonstates_covid <- setdiff(nyt_states,actual_states)

covid_complete <- covid |>
  arrange(fips,date) |>
  mutate(fips_lag = lag(fips),
         cases_lag = case_when(
           fips == fips_lag ~ lag(cases)
           ),
         new_cases = case_when(
           (cases - cases_lag) >= 0 ~ cases - cases_lag,
           TRUE ~ 0
         ),
         deaths_lag = case_when(
           fips == fips_lag ~ lag(deaths)
         ),
         new_deaths = case_when(
           (deaths - deaths_lag) >= 0 ~ deaths - deaths_lag,
           TRUE ~ 0
         )
  ) |>
  filter(date <= as.Date("2022-05-01"),
         !(state %in% nonstates_covid))


