
#### packages ####
library(tidyverse)
library(readxl)


#### naive excel file load ####
loan_files <- list.files(file.path(here::here(),"data","titleiv"))

loan_list <- list()

for (file in loan_files) {
  loan_list[[file]] <- read_excel(file.path(here::here(),"data","titleiv",file),
                                  skip = 6)
}


names(loan_list) <- str_remove_all(names(loan_list),regex("dl.dashboard.ay|\\.xls", ignore_case = TRUE)) |>
  str_replace_all("_","-") |>
  str_to_lower()




#### Resolve column names ####

names_df <- data.frame('row' = as.character(seq(1,30)))

for (df in names(loan_list)) {
  
  names_df <- cbind(names_df,names(loan_list[[df]]))
  
}

names(names_df) <- c('row',names(loan_list))

final_names <- str_remove_all(names_df$`2017-2018-q1`,"\\.\\.\\.") |>
  str_replace("((?<!\\d)6|(?<!\\d)7|(?<!\\d)8|(?<!\\d)9|(?<!\\d)10)", "-SUBSIDIZED") |>   
  str_replace("((?<!\\d)11|(?<!\\d)12|(?<!\\d)13|(?<!\\d)14|(?<!\\d)15)", "-UNSUBSIDIZED-UNDERGRADUATE") |>
  str_replace("((?<!\\d)16|(?<!\\d)17|(?<!\\d)18|(?<!\\d)19|(?<!\\d)20)", "-UNSUBSIDIZED-GRADUATE")	|>
  str_replace("((?<!\\d)21|(?<!\\d)22|(?<!\\d)23|(?<!\\d)24|(?<!\\d)25)", "-PARENT-PLUS")	|>
  str_replace("((?<!\\d)26|(?<!\\d)27|(?<!\\d)28|(?<!\\d)29|(?<!\\d)30)", "-GRAD-PLUS")				



#### Now repeat the process with the new names ####

loan_list <- list()

for (file in loan_files) {
  loan_list[[file]] <- read_excel(file.path(here::here(),"data","titleiv",file),
                                  skip = 6,
                                  col_names = final_names)
}

names(loan_list) <- str_remove_all(names(loan_list),regex("dl.dashboard.ay|\\.xls", ignore_case = TRUE)) |>
  str_replace_all("_","-") |>
  str_to_lower()

for (loan in names(loan_list)) {
  
  if(deframe(loan_list[[loan]][1,1])=="OPE ID") {
    loan_list[[loan]] <- loan_list[[loan]] |>
      slice(-1)
  }
  
}

master_loan <- map_dfr(loan_list, ~ bind_rows(.x), .id = "date")












