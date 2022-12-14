library(survival)
library(survminer)
load("data/surv_raw_df.RData")

total_surv_df <- surv_raw_df %>% 
  mutate(sold = date_end < "2022-03-01") %>% 
  select(- id, - date_start, - date_end)

rmunsold_surv_df <- surv_raw_df %>% 
  filter(date_end < "2022-03-01") %>% 
  select(- id, - date_start, - date_end)

rm1day_surv_df <- surv_raw_df %>% 
  filter(duration != 0) %>% 
  mutate(sold = date_end < "2022-03-01") %>% 
  select(- id, - date_start, - date_end)

rmboth_surv_df <- surv_raw_df %>% 
  filter(duration != 1 & date_end < "2022-03-010") %>% 
  select(- id, - date_start, - date_end)


# km --------------------------------------------------------------------------------

km <- survfit(Surv(duration, sold) ~ 1, data = total_surv_df)

ggsurvplot(km, risk.table = TRUE, surv.median.line = "hv")


# weibull ---------------------------------------------------------------------------

wb <- survreg(Surv(duration, sold) ~ 1, data = rm1day_surv_df) # zero days off

surv <- seq(.99, .01, by = -.01)
t <- predict(wb, type = "quantile", p =1 - surv, newdata = data.frame(1))
surv_wb <- data.frame(time = t, surv = surv,
                      upper = NA, lower = NA, std.err = NA)

ggsurvplot_df(fit= surv_wb, surv.geom = geom_line)


# cox -------------------------------------------------------------------------------

cxmod <- coxph(Surv(duration, sold) ~ price_diff + offer_price, data = total_surv_df)
concordance(cxmod)
# Show model coefficient
coef(cxmod)
broom::tidy(cxmod) %>% 
  mutate(estimate = exp(estimate))

newdat <- tibble(price_diff = quantile(total_surv_df$price_diff, seq(from = .1, to = .9, by = .1), na.rm = TRUE))

cxfit <- survfit(cxmod, data = total_surv_df, newdata = newdat, conf.type = "none")
ggsurvplot(cxfit)
surv_summary(cxfit) %>% 
  head()



# ranger ----------------------------------------------------------------------------

library(ranger)

total_surv_df$x <- total_surv_df$price_diff

r_fit <- ranger(Surv(duration, sold) ~ evjarat + kivitel + allapot + uzemanyag + price_diff,
                data = sample_n(drop_na(total_surv_df), 600),
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)


