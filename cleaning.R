
# load packages
library(tidyverse)
library(scales)

# load raw compiled .csv data
ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv") |>
  mutate(date = as.Date(date))


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



#### Exploratory Data Analysis ####

# filtering positively on colleges and universities
college_filtered <- ccc |>
  
  filter(
    str_detect(actors,
               regex('(universit|college)', ignore_case = TRUE)
               ) | 
      str_detect(location_detail,
                 regex('(universit|college)', ignore_case = TRUE))
  ) |>
  
  filter(date <= Sys.Date())

fips <- college_filtered |>
  pull(fips_code) |>
  unique()


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
























# filtering negatively on high, middle, elementary, prep, etc. schools
#  ccc |> 
#    dplyr::filter(str_detect(actors, 
#                             regex("student", ignore_case = TRUE)
#    ),
#    !str_detect(location_detail,
#                regex("(high|middle|elementary|public|charter) school", ignore_case = TRUE)
#    ),
#    !str_detect(location_detail,
#                regex("(high|middle|elementary|public|charter|prep.+)", ignore_case = FALSE)
#    ),
#    !str_detect(location_detail,
#                regex("highschool", ignore_case = TRUE)
#    ),
#    !str_detect(actors,
#                regex('(high|middle|elementary|public|charter) school', ignore_case = TRUE)
#    ),
#    !str_detect(claims,
#                regex('(high|middle|elementary|public|charter) school', ignore_case = TRUE)),
#    !str_detect(macroevent,
#                regex('nationalschoolwalkout', ignore_case = TRUE)
#    )
#   ) |> View()
