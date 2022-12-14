library(glmnet)
library(survival)
load("data/surv_raw_df.RData")

n_used <- 1e4
set.seed(123)

surv_cv_df <- total_surv_df %>% 
  filter(duration > 0) %>% 
  sample_n(n_used)

x_cv <- surv_cv_df %>%
  select(- duration, - sold) %>% 
  recipe() %>% 
  step_dummy(all_nominal()) %>% 
  prep() %>% 
  juice() %>% 
  as.matrix()

y_cv <- surv_cv_df %>%
  select(time = duration, status = sold) %>% 
  as.matrix()

tictoc::tic(str_c("surv_lasso", n_used))
cvfit <- cv.glmnet(x_cv, y_cv, family = "cox", type.measure = "C")
granatlib::stoc()

x <- total_surv_df %>% 
  filter(duration > 0) %>% 
  select(- duration, - sold)

y <- total_surv_df %>%
  filter(duration > 0) %>% 
  select(time = duration, status = sold) %>% 
  as.matrix()

fit_1se <- glmnet(x, y, family = "cox", lambda = cvfit$lambda.1se)
fit_min <- glmnet(x, y, family = "cox", lambda = cvfit$lambda.min)

save(x_cv, y_cv, x, y, cvfit, fit_glm, file = "data/surv_lasso.RData")