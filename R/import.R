library(tidyverse)
library(stringr)
library(readxl)

training <- read_csv("data/training.csv")
testing <- read_csv("data/testing.csv")
lookup <- read_excel("data/lookup.xlsx") %>% mutate(feature = as.integer(feature))

import <- function(files_list = as.list(list.files("data/training")), dir = "data/training") {

  names(files_list) <- files_list
  
  df <- map_df(files_list, function(file) {
    
    read_csv(paste0(dir, file))
    
  }, .id = "files")
  
  df %>%
    mutate(files = substr(files, 1, str_length(files) - 4)) %>%
    separate(files, c("experiment", "ExperimentID"), "-") %>%
    select(-experiment) %>%
    mutate(ExperimentID = as.integer(ExperimentID))

}

train_df <- import(files_list = as.list(list.files("data/training")), dir = "data/training/")

test_df <- import(files_list = as.list(list.files("data/testing")), dir = "data/testing/")

all_df <-
  train_df %>% mutate(set = "train") %>%
  bind_rows(test_df %>% mutate(set = "test"))

all_df <- all_df %>% 
  group_by(ExperimentID) %>%
  mutate(index = row_number()) %>%
  ungroup() %>%
  group_by(set, ExperimentID) %>%
  mutate(set_index = row_number()) %>%
  ungroup() %>%
  arrange(ExperimentID, desc(set))


#%>%
#  gather(feature_id, val, -set, -index) %>%
#  left_join(lookup, "feature_id") %>%
#  spread(feature, val)


