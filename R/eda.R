azs_1 <- all_df %>% 
  select(index, ExperimentID, set, f1 = f_101, f2 = f_102, f3 = f_103, f4 = f_104, f5 = f_105) %>%
  gather(feature, val, -index, -ExperimentID, -set) %>%
  mutate(sensor = "azs_1")

azs_2 <- all_df %>%
  select(index, ExperimentID, set, f1 = f_146, f2 = f_147, f3 = f_148, f4 = f_149, f5 = f_150) %>%
  gather(feature, val, -index, -ExperimentID, -set) %>%
  mutate(sensor = "azs_2") 

azs_1 %>%
  bind_rows(azs_2) %>%
  filter(ExperimentID == 1L, set == "train") %>%
  ggplot(., aes(x = index, y = val, color = sensor)) + 
  geom_line() +
  facet_wrap(~ feature, scales = "free_y")

# correlation between front and rear for same feature and same sensor
ccf(train_df %>% filter(ExperimentID == 1L) %>% .$f_105, train_df %>% filter(ExperimentID == 1L) %>% .$f_150)

# train and test data for 1 feature and experiment
ggplot(all_df %>% filter(ExperimentID == 1), aes(x = index, y = f_101, color = set)) + geom_line()

azs_1 %>%
  bind_rows(azs_2) %>%
  filter(ExperimentID %in% c(1L)) %>%
  ggplot(., aes(x = as.factor(ExperimentID), y = val, color = sensor)) + 
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y")
