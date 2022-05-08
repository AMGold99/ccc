library(tidyverse)
library(googlesheets4)
library(readxl)

ss <- "16rhromEvV09n63UWA43-o2FdvqqLYdNoNxKX0_vGwJw"

zip_crosswalk <- read_xlsx("data/ZIP_COUNTY_122021.xlsx")

zip_join <- zip_crosswalk |>
  select(zip, county) |>
  rename(fips = "county")


colleges <- read_sheet(ss,
                       sheet = "IPEDS Inst Directory Data")


nonstates_college <- setdiff(unique(college_complete$STABBR), 
                     unique(fips_df$state)
                     )

college_fips <- colleges |>
  select(!FIPS) |>
  filter(STABBR != "PR") |>
  mutate(zip = str_sub(ZIP, 1, 5)) |>
  mutate(zip = case_when(
    str_length(zip) == 4 ~ paste0("0",zip),
    TRUE ~ as.character(zip)
  )) |>
  left_join(zip_join) |>
  filter(!(STABBR %in% nonstates_college))


city_join <- zip_crosswalk |>
  rename(fips = "county",
         CITY = "usps_zip_pref_city",
         STABBR = "usps_zip_pref_state")
  


fips_find <- college_fips[is.na(college_fips$fips),
                 c('INSTNM','CITY','STABBR','fips','ZIP','zip')] |> 
  mutate(CITY = str_to_upper(CITY)) |>
  left_join(city_join, by = c("CITY","STABBR")) |> 
  select(!ends_with("ratio")) |> 
  select(INSTNM,CITY,STABBR,fips.y) |>
  filter(!is.na(fips.y)) |>
  distinct()

# sheet_write(fips_find,
#             ss = ss,
#             sheet = "fips_find")

fips_final <- read_sheet(ss = ss,
                         sheet = "fips_find") |>
  filter(candidate == "x") |>
  select(!c(candidate, CITY, STABBR))


college_complete <- college_fips |>
  left_join(
    fips_final, by = "INSTNM"
  ) |>
  mutate(fips = case_when(
    is.na(fips) ~ fips.y,
    TRUE ~ fips
  )) |>
  distinct() |>
  filter(!duplicated(INSTNM)) |>
  select(!fips.y)|>
  mutate(ZIP = unlist(ZIP))
