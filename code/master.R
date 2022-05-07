conus_fips <- fips_df |>
  filter(!is.na(state)) |>
  pull(fips)

date_range <- seq(as.Date("2017-01-20"),as.Date("2022-05-01"), by = "day")

expand.grid(
  date = date_range,
  fips = conus_fips
)
