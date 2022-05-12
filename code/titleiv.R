
#### LOANS ####
 
#### packages ####
library(tidyverse)
library(readxl)


#### naive excel file load ####
loan_files <- list.files(file.path(here::here(),"data","titleiv_loan"))

loan_list <- list()

for (file in loan_files) {
  loan_list[[file]] <- read_excel(file.path(here::here(),"data","titleiv_loan",file),
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
  str_replace("((?<!\\d)6|(?<!\\d)7|(?<!\\d)8|(?<!\\d)9|(?<!\\d)10)", "_sub") |>   
  str_replace("((?<!\\d)11|(?<!\\d)12|(?<!\\d)13|(?<!\\d)14|(?<!\\d)15)", "_unsub_under") |>
  str_replace("((?<!\\d)16|(?<!\\d)17|(?<!\\d)18|(?<!\\d)19|(?<!\\d)20)", "_unsub_grad")	|>
  str_replace("((?<!\\d)21|(?<!\\d)22|(?<!\\d)23|(?<!\\d)24|(?<!\\d)25)", "_parentplus")	|>
  str_replace("((?<!\\d)26|(?<!\\d)27|(?<!\\d)28|(?<!\\d)29|(?<!\\d)30)", "_gradplus") |>
  str_replace("# of ","n_") |>
  str_replace("\\$ of ", "dol_") |>
  str_replace("Loans Originated", "loan_orig") |>
  str_replace("Disbursements","disburse") |>
  str_replace("R","r")



#### Now repeat the process with the final_names ####

loan_list <- list()

for (file in loan_files) {
  loan_list[[file]] <- read_excel(file.path(here::here(),"data","titleiv_loan",file),
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


# create crosswalk between quarters and exact start/end dates
ref_list <- list()

for (file in loan_files) {
  df <- read_excel(file.path(here::here(),"data","titleiv_loan",file),
                                  col_names = final_names)
  
  if(str_detect(file,"2016")) {
    
    df1 <- df |>
      slice(2)
  } else {
    
    df1 <- df |> 
      slice(3)
  }
  
  ref_list[[file]] <- df1 |>
    select(1) |>
    mutate(dateRange = str_remove_all(`OPE ID`, "Award Year Quarterly Activity[:blank:]|\\(|\\)"),
           .keep = "unused") |>
    separate(col = `dateRange`, 
             into = c("start_date","end_date"), 
             sep = "-") |>
    mutate(quarter = str_remove_all(file, regex("dl.dashboard.ay|\\.xls", ignore_case = TRUE)),
           quarter = str_to_lower(str_replace_all(quarter,"_","-")),
           start_date = lubridate::mdy(start_date),
           end_date = lubridate::mdy(end_date))
}

names(ref_list) <- str_remove_all(names(ref_list),regex("dl.dashboard.ay|\\.xls", ignore_case = TRUE)) |>
  str_replace_all("_","-") |>
  str_to_lower()

date_crosswalk <- bind_rows(ref_list)

#### Collate master dataframe ####
for (i in seq_len(length(loan_list))) {
  loan_list[[i]] <- loan_list[[i]] |>
    mutate_if(is.numeric, as.character)
}


master_loan <- map_dfr(loan_list, ~ bind_rows(.x), .id = "quarter") |>
  left_join(
    date_crosswalk
  ) |>
  relocate(start_date, .after = "quarter") |>
  relocate(end_date, .after = "start_date") |>
  rename(OPEID = "OPE ID")

# make sure all OPEID lengths are 8.
unique(str_length(master_loan$OPEID))








