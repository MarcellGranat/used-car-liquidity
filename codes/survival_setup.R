load("data/setup.RData") # training_df
load("data/fair_prices_df.RData")
load("data/cars_data_imputed.RData")
load("data/unsold_cars.RData")

fair_price_df <- fair_prices_df %>% 
  select(id, fit) %>% 
  unnest() %>% 
  ungroup() %>% 
  select(id, fair_price = .pred) %>% 
  mutate(
    fair_price = exp(fair_price * sd(log(training_df$price)) + mean(log(training_df$price)))
  )

duration_df <- prices_df %>%
  arrange(date) %>% 
  group_by(id) %>% 
  slice(1, n()) %>% # first and last
  transmute(name = c("start", "end"), date) %>% 
  pivot_wider(values_from = date, names_prefix = "date_") %>% 
  ungroup() %>% 
  filter(date_start != min(date_start)) %>% 
  mutate(
    duration = date_end  - date_start,
    duration = as.numeric(duration),
  )

offer_price_df <- prices_df %>% 
  arrange(date) %>% 
  group_by(id) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  select(id, offer_price = price)

total_cars_data <- cars_data_imputed %>% 
  bind_rows(cars_unsold_df)

total_accessories_df <- accessories_df %>% 
  bind_rows(accessories_unsold_df)

surv_raw_df <- duration_df %>% 
  left_join(offer_price_df) %>% 
  left_join(fair_price_df) %>% 
  left_join(total_cars_data) %>% 
  left_join(total_accessories_df) %>% 
  drop_na(offer_price) %>% 
  mutate(price_diff = offer_price / fair_price)

total_surv_df <- surv_raw_df %>% 
  mutate(sold = date_end < "2022-03-01") %>% 
  select(- id, - date_start, - date_end) %>% 
  drop_na()


save(surv_raw_df, total_surv_df, file = "data/surv_raw_df.RData")


