
# load packages
library(tidyverse)
library(scales)
library(usmap)

# load raw compiled .csv data
ccc <- readr::read_csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")


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
missing |>
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
  )

fips <- college_filtered |>
  pull(fips_code) |>
  unique()

usmap::plot_usmap(include = fips)





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