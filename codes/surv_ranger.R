library(survival)
library(survminer)
library(ranger)
load("data/surv_raw_df.RData")


set.seed(123)

n_used <- 2e4

tictoc::tic(str_c("surv_ranger_", n_used))

r_fit <- ranger(Surv(duration, sold) ~ .,
                data = sample_n(total_surv_df, n_used),
                num.trees = 50,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

granatlib::stoc()

save(r_fit, file = "data/surv_ranger.RData")

# Without fair_price ----------------------------------------------------------------


set.seed(123)

r_fit_wo_fair <- ranger(Surv(duration, sold) ~ .,
                data = sample_n(select(total_surv_df, - fair_price), n_used),
                num.trees = 50,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

save(r_fit_wo_fair, file = "data/surv_ranger_wo_fair.RData")


