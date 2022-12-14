load("data/cars_data_imputed.RData")
load("data/setup.RData")
load("app/last_fit.RData")
load("data/mca.RData")

combined_df <- last_price_df %>% 
  select(id, date = end, price) %>% 
  arrange(desc(date)) %>%
  filter(!duplicated(id)) %>% # last observed price by car
  left_join(
    select(cars_data_imputed, id, brand, where(~ is.numeric(.) | n_distinct(.) < 10)) # variables not in MCA
  ) %>%
  filter(!is.na(kilometerora_allasa) & !is.na(price)) %>% # probably just add
  left_join(mca_factor_df) # variables from MCA

price_change_df <- prices_df %>% 
  group_by(id) %>% 
  summarise(
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    price_change = max_price / min_price
  )

modeled_df <- combined_df %>% # named as df in the prev codes 
  # used for imputation
  anti_join(
    price_change_df %>% 
      filter(price_change > 5) %>% # unrealistic jump in price ~ 100 cars
      select(id)
  ) %>% 
  filter(price <= 3e7 & price >= 3e5 & !is.na(fct_1)) %>%  # unrealistic prices ~ 15 000 cars
  na.omit() %>% 
  filter(allapot != "Other") # occured only 4 times, but 0 before 2021-07-01 >> error in prediction

rm(list = setdiff(ls(), c("modeled_df", "last_fit", "rec_prep", "training_df")))

load("app/surv_setup.RData")

save.image(file = "app_data.RData")


test_rows <- map_df(df, sample, size = 1) %>% 
  mutate_at(sample(1:105, 70), function(x) NA)

known_data_df <- select(test_rows, brand, evjarat)

semi_join(df, known_data_df)

imputed_df <- bind_rows(test_rows, semi_join(df, known_data_df)) %>% 
  select(- id, - date) %>% 
  mice::mice(method = "cart", m = 1) %>% 
  mice::complete() %>% 
  slice(1) %>% 
  tibble()

predicted_fair_price <- imputed_df %>% 
  bake(object = rec_prep) %>% 
  predict(object = last_fit) %>% 
  transmute(
    fair_price = exp(.pred * sd(log(training_df$price)) + mean(log(training_df$price)))
  )
  
