library(forecast)
library(broom)

mdl_df <- all_df %>% filter(
    ExperimentID == 1L,
    set == "train"
    )

test_df_actuals <- all_df %>%
  filter(
    set == "test",
    set_index <= 10L
  )

ggplot(mdl_df, aes(x = index, y = f_105)) + geom_line()

mdl <- auto.arima(mdl_df$f_105)

plot(forecast(mdl))

preds <- tibble(pred = as.numeric(predict(mdl, 10L)$pred))

test_y <- test_df_actuals %>% filter(
  ExperimentID == 1L,
  set == "test"
) %>%
  select(
    ExperimentID,
    index,
    set_index,
    actual = f_105
    )

results <- bind_cols(test_y, preds) %>%
  mutate(
    error = actual - pred,
    error_sq = error ^ 2
    ) 

ggplot(results, aes(x = error)) + geom_histogram()

ggplot(results, aes(x = ExperimentID, y = error)) + geom_boxplot()

ggplot(results, aes(x = set_index, y = error)) + geom_line()


models <- all_df %>%
  select(ExperimentID, set, index, set_index, f_105) %>%
  nest(-ExperimentID, -set) %>%
  filter(set == "train") %>%
  #filter(ExperimentID %in% c(1L, 2L)) %>%
  mutate(model = map(data, ~ auto.arima(y = .$f_105))) %>%
  mutate(pred = map(model, ~ as.numeric(predict(., 10L)$pred)))

results <- models %>% unnest(pred) %>% select(pred) %>% bind_cols(test_df_actuals) %>%
  mutate(
    error = f_105 - pred,
    error_sq = error ^ 2
  )

ggplot(results, aes(x = as.factor(ExperimentID), y = error)) + geom_boxplot() + coord_flip()

results %>%
  filter(error > 0.02) %>%
  group_by(ExperimentID) %>%
  count() %>%
  arrange(desc(n))


ggplot(all_df %>% filter(ExperimentID == 168L), aes(x = index, y = f_105, color = set)) + geom_line()
ggplot(all_df %>% filter(ExperimentID == 8L), aes(x = index, y = f_105, color = set)) + geom_line()
ggplot(all_df %>% filter(ExperimentID == 155L), aes(x = index, y = f_105, color = set)) + geom_line()

ggplot(all_df %>% filter(ExperimentID == 168L), aes(x = index, y = f_105, color = set)) + geom_line()

all_df %>% filter(ExperimentID == 168L) %>%
  gather(feature_id, val, -index, -set_index, -ExperimentID, -set) %>%
  left_join(lookup, "feature_id") %>%
  filter(feature == 5L) %>%
  ggplot(., aes(x = index, y = val, color = set)) +
  geom_line() +
  facet_wrap(~ sensor, scales = "free_y")


# nested by feature
models <- all_df %>%
  gather(feature, val, -ExperimentID, -index, -set_index, -set) %>%
  nest(-ExperimentID, -set, -feature) %>%
  filter(set == "train") %>%
  filter(ExperimentID %in% c(1L, 2L)) %>%
  mutate(model = map(data, ~ possibly(auto.arima(y = .)))) %>%
  mutate(pred = map(model, ~ as.numeric(predict(., 10L)$pred)))

