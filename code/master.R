library(tidyverse)

conus_fips <- fips_df |>
  filter(!is.na(state)) |>
  mutate(fips = case_when(
    str_length(fips) == 4 ~ paste0("0",fips),
    TRUE ~ as.character(fips)
  )) |>
  pull(fips)

date_range <- seq(as.Date("2017-01-20"),as.Date("2022-05-01"), by = "day")



# add static census variables to this dataframe
master_crosssection <- tibble(fips = conus_fips) |> 
  left_join(college_complete) |>
  mutate(college_present = case_when(
    is.na(INSTNM) ~ 0,
    !is.na(INSTNM) ~ 1
  )) |>
  relocate(college_present, .after = "fips") |>
  group_by(fips) |>
  summarise(ncollege = sum(college_present))


master_panel <- expand.grid(
  date = date_range,
  fips= conus_fips
) |>
  tibble() |>
  
  # merge daily covid cases and deaths; should we replace NA with 0
  left_join(covid_complete) |>
  select(!c(fips_lag,cases_lag,deaths_lag,deaths,cases,county,state)) |>
  
  # merge daily college protests
  left_join(
    ccc_filtered |>
      group_by(fips,date) |>
      count()
  ) |>
  
  # create count and binary indicators of college protest
  mutate(pfiltercount = replace_na(n, 0), .keep = "unused",
         pfilterbin = case_when(
           pfiltercount > 0 ~ 1,
           pfiltercount == 0 ~ 0
         ))


# p[filter]count: how many protests occurred
# p[filter]bin: did any protests occurred


