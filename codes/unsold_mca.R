load("data/unsold_cars.RData")
load("data/mca.RData")
# TODO fit mca for unsold cars

additional_factor_df <- cars_unsold_df %>%
  dplyr::select(id, where(is.factor)) %>%
  dplyr::select(id, where(~ n_distinct(.) >= 10))

omit_cars <- additional_factor_df %>% 
  pivot_longer(-1) %>% 
  group_by(name, value) %>% 
  filter(n() <= 10) %>% 
  ungroup()

factor_df <- accessories_unsold_df %>% 
  dplyr::select(id, where(~ is.logical(.) && n_distinct(.) != 0)) %>% 
  left_join(additional_factor_df) %>%
  mutate_at(-1, as.factor) %>% 
  na.omit()

aux_mca_lvl_df <- fit_mca$cs %>% 
  rownames() %>% 
  enframe(NULL, "raw") %>% 
  mutate(
    lvl = case_when(
      str_detect(raw, "TRUE") ~ "TRUE",
      str_detect(raw, "FALSE") ~ "FALSE",
      str_detect(raw, "brand.") ~ str_remove(raw, "brand."),
    ),
    fct = ifelse(str_starts(raw, "brand"), "brand", str_remove(raw, ".FALSE|.TRUE"))
  )

mca_unsold_df <- factor_df %>% 
  select(- id) %>% 
  imap_dfc(~ {
    lvls <- aux_mca %>% 
      filter(fct == .y) %>% 
      pull(lvl)
    
    factor(.x, levels = lvls)
  }) %>% 
  predict(object = fit_mca) %>% 
  data.frame() %>% 
  rename_all(str_replace, "X", "fct_") %>% 
  bind_cols(factor_df["id"]) %>% 
  select(id, everything())

save(mca_unsold_df, file = "data/mca_unsold_df")

