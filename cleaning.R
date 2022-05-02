
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
         )


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


#### COLLEGE_FILTERED ####

# filtering positively on colleges and universities
college_filtered <- ccc |>
  
  filter(
    str_detect(actors,
               regex('(universit|college)', ignore_case = TRUE)
               ) | 
      str_detect(location_detail,
                 regex('(universit|college)', ignore_case = TRUE))
  ) |>
  
  filter(date <= Sys.Date()) |>
  
  filter(!(state_code %in% c("66","72",NA))) |>
  filter(online != 1)



#### FIPS CALIFORNIA ####

ccc_ca <- college_filtered |>
  filter(state == "CA") |>
  select(fips_code)


ccc_ca_count <- college_filtered |>
  filter(state == "CA") |>
  group_by(fips_code) |>
  summarise(count = n()) |>
  rename(fips = "fips_code")

fips_df <- readr::read_csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv")

fips_ca <- fips_df |>
  filter(state == "CA") |>
  mutate(fips = case_when(
    nchar(fips) == 4 ~ paste0("0",fips),
    TRUE ~ as.character(fips)
    )
  ) |>
  mutate(ccc = case_when(
    fips %in% ccc_ca$fips_code ~ "1",
    TRUE ~ "0"
    )
  ) |>
  
  left_join(
    ccc_ca_count
  ) |>
  
  mutate(count = replace_na(count,0))


 



#### CLAIMS FILTERING ####
ccc |>
  filter(str_detect(claims, regex("(universit|college)", ignore_case = TRUE)) & 
           !str_detect(actors, regex("(universit|college)", ignore_case = TRUE)) &
           !str_detect(location_detail, regex("universit|college", ignore_case = TRUE))
           ) |> 
  select(actors, location_detail,claims)




#### VIZ ####

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
college_timeline_plot <- college_filtered |>
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







