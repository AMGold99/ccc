
# load packages
library(tidyverse)
library(scales)
library(usmap)


#### LOAD DATA ####

# load raw compiled .csv data
ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv") |>
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         fips_code = case_when(
           nchar(fips_code) == 4 ~ paste0("0",fips_code),
           TRUE ~ as.character(fips_code)
           ),
         state_code = str_sub(fips_code, 1, 2)
         ) |>
  filter(date <= as.Date("2022-05-01"))


#### MISSING VALUES ####

# check missing values
na_list <- purrr::map(names(ccc), ~ sum(is.na(ccc[[.x]]))
                      )
names(na_list) <- names(ccc)

missing <- bind_cols(na_list) |> 
  
  select(!starts_with("source")) |>
  
  pivot_longer(
    cols = everything(),
    names_to = 'var',
    values_to = 'missing_count'
  ) |>
  
  dplyr::mutate(missing_prop = missing_count/nrow(ccc))#|>

# plot missing value proportions
missing_plot <- missing |>
  ggplot(aes(reorder(var,-missing_prop),missing_prop))+
  geom_hline(yintercept = seq(0,1, by = 0.1), colour = 'grey')+
  geom_vline(xintercept = missing$var, colour = "lightgrey")+
  geom_point()+
  coord_flip()+
  
  scale_y_continuous(breaks = seq(0,1,by=0.1))+
  
  
  labs(
    title = 'CCC Missing Values',
    subtitle = "Not shown: source_n (mostly missing)",
    x = NULL,
    y = "Missing Values (Proportion)"
  )+
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA)
  )


#### CCC_filtered ####

# filtering positively on colleges and universities
ccc_filtered <- ccc |>
  
  # filter on actors, location_detail, claims
  filter(
    str_detect(actors,
               regex('(universit|college)', ignore_case = TRUE)
               ) | 
    str_detect(location_detail,
                regex('(universit|college)', ignore_case = TRUE)) |
    str_detect(claims,
               regex('(universit|college)', ignore_case = TRUE))
  ) |>
  
  filter(date <= as.Date("2022-05-01")) |>
  
  filter(!(state_code %in% c("66","72",NA))) |>
  filter(online != 1) |>
  rename(fips = "fips_code") |>
  tibble()



#### PROTEST MAPS ####

# fips associated with any protests
ccc_fips <- ccc |>
  select(fips_code) |>
  rename(fips = "fips_code")

# fips associated with college protests
ccc_filtered_fips <- ccc_filtered |>
  select(fips)

# count of any protest by fips county
ccc_count <- ccc |>
  group_by(fips_code) |>
  summarise(total_count = n()) |>
  rename(fips = "fips_code")

# count of college protest by fips county
ccc_filtered_count <- ccc_filtered |>
  group_by(fips) |>
  summarise(filtered_count = n())


# load fips reference dataframe
fips_df <- readr::read_csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv")


# calculate binary and count vars for protest occurrence
fips_protest <- fips_df |>
  filter(state %in% fips_info()$abbr) |>
  mutate(
    fips = case_when(
          nchar(fips) == 4 ~ paste0("0",fips),
          TRUE ~ as.character(fips)),
    pbin = case_when(
          fips %in% ccc_fips$fips ~ "1",
          TRUE ~ "0"),
    pfilterbin = case_when(
      fips %in% ccc_filtered_fips$fips ~ "1",
      TRUE ~ "0"
    )
  ) |>
  
  left_join(ccc_count) |>
  
  left_join(ccc_filtered_count) |>
  
  mutate(pcount = replace_na(total_count, 0),
         pfiltercount = replace_na(filtered_count, 0), 
         .keep = "unused")



#### VIZ ####

# Any Protest (not filtered) map
plot_usmap(data = fips_protest, values = "pbin") +
  
  scale_fill_manual(
    values = c(
      `1` = "royalblue4",
      `0` = "white",
      `NA` = "grey25"
    ),
    labels = c('Yes','No',"No Data")
  ) +
  
  labs(
    title = "Protests at College and Universities",
    subtitle = glue::glue("Jan 2017 to May 2022 (n = {nrow(ccc)})"),
    fill = "Any Protest \nOccurred?"
  ) +
  
  theme(
    text = element_text(size = 12),
    legend.position = c(1,0.5)
  )

# College Protest (filtered) map
plot_usmap(data = fips_protest, values = "pfilterbin") +
  
  scale_fill_manual(
    values = c(
      `1` = "royalblue4",
      `0` = "white",
      `NA` = "grey25"
    ),
    labels = c('Yes','No',"No Data")
  ) +
  
  labs(
    title = "Protests at College and Universities",
    subtitle = glue::glue("Jan 2017 to May 2022 (n = {nrow(ccc_filtered)})"),
    fill = "College Protest \nOccurred?"
  ) +
  
  theme(
    text = element_text(size = 12),
    legend.position = c(1,0.5)
  )


# timeline plot
timeline_plot <- ccc |>
  filter(date <= Sys.Date())|>
  group_by(date) |>
  summarise(count = n()) |>
  
  filter(count < 1000) |>

  ggplot(aes(date, count))+
  geom_hline(yintercept = c(0, 250, 500, 750, 1000), colour = "lightgrey")+
  geom_vline(xintercept = c(as.Date("2018-03-24"), as.Date("2020-05-25")), 
             colour = "red", 
             linetype = "dashed")+
  geom_bar(stat = 'identity')+
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(limits = c(0, 1000), expand = expansion(mult = 0.05))+
  
  annotate("text",
           label = "Murder of\n George Floyd",
           x = as.Date("2020-12-01"),
           y = 880,
           colour = "red")+
  
  annotate("text",
           label = "March for\n Our Lives",
           x = as.Date("2017-11-01"),
           y = 600,
           colour = "red")+
  
  labs(
    title = "CCC Protests Over Time",
    subtitle = "Excluding several March for Our Lives days with >3000 events",
    x = NULL,
    y = "# Protest Events"
  )+
  
  theme(
    panel.background = element_blank()
  )


#college timeline plot
college_timeline_plot <- ccc_filtered |>
  group_by(date) |>
  summarise(count = n()) |> 
  filter(count < 50) |>
  
  ggplot(aes(date,count))+
  geom_hline(yintercept = c(0,10,20,30,40,50), colour = "lightgrey")+
  geom_vline(xintercept = c(as.Date("2018-03-24"), as.Date("2020-05-25")), 
             colour = "red", 
             linetype = "dashed")+
  geom_bar(stat = 'identity')+
  
  scale_y_continuous(limits=c(0,50))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  
  labs(
    title = "Protests Over Time: College/University Only",
    subtitle = "Excluding several March for Our Lives days with >50 events",
    y = "# Protest Events"
  )+
  
  annotate("text",
           label = "Murder of\n George Floyd",
           x = as.Date("2020-12-01"),
           y = 45,
           colour = "red")+
  
  annotate("text",
           label = "March for\n Our Lives",
           x = as.Date("2017-11-01"),
           y = 35,
           colour = "red")+

  theme(
    panel.background = element_blank()
  )







save.image("ccc.RData")







