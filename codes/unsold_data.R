source("codes/utils.R", encoding = "utf-8")
plan(multisession, workers = 8)

hasznaltauto_wd <- getwd() %>% 
  # MarcellGranat/hasznaltauto repo is required to be in the parent directory of project
  # ^ refresh the data before run
  str_remove("automobile-survival") %>% 
  str_c("hasznaltauto/")

cars_data <- list.files(str_c(hasznaltauto_wd, "data/cars_data/"), full.names = T) %>%
  future_map(function(x) {
    read_rds(x) %>% 
      select(url_to_car, data) %>% 
      filter(!duplicated(url_to_car)) %>% 
      unnest() %>% 
      unnest() %>% 
      set_names("id", "name", "value") %>%
      mutate(
        brand = gsub(".*szemelyauto/", "", id),
        brand = gsub("/.*", "", brand),
        id = gsub(".*-", "", id),
        id = gsub(".*/", "", id)
      ) %>%
      unique()
  }) %>% 
  bind_rows() %>% 
  filter(!duplicated(str_c(id, name))) %>% 
  pivot_wider()

cars_data <- cars_data %>%
  janitor::clean_names() %>%
  select(- jarmueloelet) %>% # new variable
  mutate(
    evjarat = gsub("/.*", "", evjarat),
    evjarat = as.numeric(evjarat), 
    kilometerora_allasa = str_remove_all(kilometerora_allasa, "\\D"), 
    kilometerora_allasa = as.numeric(kilometerora_allasa),
    szallithato_szem_szama = str_remove_all(szallithato_szem_szama, "\\D"), 
    szallithato_szem_szama = as.integer(szallithato_szem_szama),
    szallithato_szem_szama = ifelse(szallithato_szem_szama > 60, NA, szallithato_szem_szama),
    ajtok_szama = as.numeric(ajtok_szama),
    ajtok_szama = ifelse(ajtok_szama == 1, NA, ajtok_szama),
    ajtok_szama = as.factor(ajtok_szama),
    szin = str_to_lower(szin),
    szin = case_when(
      str_detect(szin, "kék") ~ "kek", 
      str_detect(szin, "ibolya") ~ "kek", 
      str_detect(szin, "türkiz") ~ "kek", 
      str_detect(szin, "piros") ~ "piros", 
      str_detect(szin, "vörös") ~ "piros",
      str_detect(szin, "bordó") ~ "piros",
      str_detect(szin, "fekete") ~ "fekete",
      str_detect(szin, "szürke") ~ "szurke",
      str_detect(szin, "ezüst") ~ "szurke",
      str_detect(szin, "fehér") ~ "feher",
      str_detect(szin, "barna") ~ "barna",
      str_detect(szin, "homok") ~ "barna",
      str_detect(szin, "pezsgő") ~ "barna",
      str_detect(szin, "bézs") ~ "barna",
      str_detect(szin, "vaj") ~ "barna",
      str_detect(szin, "zöld") ~ "zold",
      str_detect(szin, "sárga") ~ "sarga",
      str_detect(szin, "narancs") ~ "sarga",
      str_detect(szin, "lila") ~ "lila",
      TRUE ~ "other"
    ),
    szin = fct_lump(szin, prop = .03, other = "other"),
    klima_fajtaja = ifelse(is.na(klima_fajtaja), "Nincs klíma", klima_fajtaja),
    klima_fajtaja = ifelse(klima_fajtaja == "Hőszivattyús klíma", "Automata klíma", klima_fajtaja), # merge top categories <- prop(hoszivattyus < .01)
    hengerurtartalom = str_remove_all(hengerurtartalom, "\\D"),
    hengerurtartalom = as.numeric(hengerurtartalom),
    teljesitmeny = gsub(".*kW, ", "", teljesitmeny),
    teljesitmeny = str_remove_all(teljesitmeny, "\\D"),
    teljesitmeny = as.numeric(teljesitmeny),
    muszaki_vizsga_ervenyes = gsub("/.*", "", muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = as.numeric(muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = muszaki_vizsga_ervenyes - 2021,
    muszaki_vizsga_ervenyes = ifelse(muszaki_vizsga_ervenyes < 0, "none", muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = as.character(muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = ifelse(is.na(muszaki_vizsga_ervenyes), "none", muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = factor(muszaki_vizsga_ervenyes, levels = c("none", as.character(0:15)), ordered = TRUE),
    sajat_tomeg = str_remove_all(sajat_tomeg, "\\D"),
    sajat_tomeg = as.numeric(sajat_tomeg),
    teljes_tomeg = str_remove_all(teljes_tomeg, "\\D"),
    teljes_tomeg = as.numeric(teljes_tomeg),
    csomagtarto = str_remove_all(csomagtarto, "\\D"),
    csomagtarto = as.numeric(csomagtarto),
    brand = fct_lump(brand, n = 50),
    nyari_gumi_meret2 = gsub(".*/", "", nyari_gumi_meret),
    nyari_gumi_meret2 = str_sub(nyari_gumi_meret2, end = 2),
    nyari_gumi_meret3 = gsub(".*R", "", nyari_gumi_meret),
    nyari_gumi_meret = gsub("/.*", "", nyari_gumi_meret),
    across(starts_with("nyari_gumi_meret"), str_trim),
    across(starts_with("nyari_gumi_meret"), as.integer),
    sebessegvalto_fokozatszam = str_remove_all(sebessegvalto_fajtaja, "\\D"),
    sebessegvalto_fokozatszam = as.numeric(sebessegvalto_fokozatszam),
    sebessegvalto_fokozatszam = ifelse(is.na(sebessegvalto_fokozatszam), 0, sebessegvalto_fokozatszam),
    sebessegvalto_fajtaja = case_when(
      str_detect(sebessegvalto_fajtaja, "tiptronic") ~ "tiptronic",
      str_detect(sebessegvalto_fajtaja, "zekvenciális") ~ "szekvenciális",
      str_detect(sebessegvalto_fajtaja, "anuális") ~ "manuáis",
      str_detect(sebessegvalto_fajtaja, "utomata") ~ "automata",
      TRUE ~ "egyéb"
    )
  ) %>% 
  select(-(alaptipus_ara:atveheto),
         -(finanszirozas:jarmu_adatok),
         -(karpit_szine_1:finanszirozas_kalkulator_hirdetes),
         - motor_adatok, -abroncs, -okmanyok, -okmanyok_jellege, -vetelar
  ) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(id = as.character(id)) %>% 
  mutate(across(is.factor & !starts_with("brand"), ~ fct_lump(., prop = .03))) %>% 
  filter(!is.na(kilometerora_allasa) & !is.na(evjarat)) %>% # evjarat ~ 1000 cars
  filter(kilometerora_allasa > 0) %>% 
  mutate_if(is.numeric, ~ as.integer(round(.)))

accessories_df <- list.files(str_c(hasznaltauto_wd, "data/cars_data/"), full.names = T) %>%
  future_map(function(x) {
    read_rds(x) %>% 
      select(id = url_to_car, name = other_data)
  }) %>% 
  bind_rows() %>% 
  mutate(
    id = gsub(".*-", "", id),
    id = gsub(".*/", "", id)
  ) %>% # TODO new
  filter(!duplicated(id)) %>% 
  unnest() %>% 
  distinct() %>% 
  mutate(value = TRUE) %>% 
  pivot_wider() %>% 
  mutate_at(-1, ~ ifelse(is.na(.), FALSE, .)) %>% 
  janitor::clean_names() %>% 
  rename(hangszoro = x6_hangszoro) %>% 
  select(-(x5:terkep)) %>% 
  {select(., setdiff(names(.), str_c("x", 0:100)))} %>% 
  select(!ends_with("percent_tol_elviheto")) %>% 
  left_join(x = cars_data["id"])

prices_df <- list.files(str_c(hasznaltauto_wd, "/data/available_cars/"), full.names = T) %>% 
  future_map(~ mutate(read_rds(.), date = .)) %>% 
  bind_rows() %>% 
  rename(id = url_to_car) %>% 
  mutate(
    id = gsub(".*-", "", id),
    id = gsub(".*/", "", id),
    date = str_remove_all(date, "\\D"),
    date = lubridate::ymd(date),
    price = str_remove_all(price, "\\D"),
    price = as.numeric(price)
  )

last_price_df <- prices_df %>% 
  arrange(date) %>% 
  group_by(id) %>% 
  summarise(
    start = min(date), end = max(date), price = last(price)
  ) %>% 
  ungroup() %>% 
  left_join(x = cars_data["id"])

library(tidymodels)

knn_impute <- function(.data, formula, ...) {
  outcome <- gsub("~.*", "", formula) %>% 
    str_trim()
  
  predictors <- gsub(".* ~", "", formula) %>% 
    str_split("\\+") %>% 
    reduce(c) %>% 
    str_trim()
  
  recipe(.data) %>% 
    step_normalize(all_numeric(), -outcome) %>% 
    step_impute_knn(outcome, impute_with = predictors, ...) %>% 
    prep(retain = TRUE) %>% 
    juice() %>% 
    select(outcome)
}

imputed_df <- future_map(.progress = TRUE,
                         c(
                           "nyari_gumi_meret ~ brand + evjarat + sajat_tomeg",
                           "nyari_gumi_meret2 ~ brand + evjarat + sajat_tomeg",
                           "nyari_gumi_meret3 ~ brand + evjarat + sajat_tomeg",
                           "szallithato_szem_szama ~ brand + evjarat + sajat_tomeg",
                           "ajtok_szama ~ brand + evjarat + sajat_tomeg + szallithato_szem_szama",
                           "hengerurtartalom ~ brand + evjarat + szallithato_szem_szama",
                           "henger_elrendezes ~ brand + evjarat + hengerurtartalom + teljesitmeny",
                           "hajtas ~ brand + evjarat + teljesitmeny + uzemanyag",
                           "teljesitmeny ~ brand + evjarat + hengerurtartalom",
                           "sajat_tomeg ~ brand + evjarat + teljesitmeny + szallithato_szem_szama + ajtok_szama",
                           "teljes_tomeg ~ brand + evjarat + teljesitmeny + szallithato_szem_szama + ajtok_szama",
                           "uzemanyag ~ brand + evjarat + teljesitmeny + hengerurtartalom",
                           "csomagtarto ~ brand + evjarat + szallithato_szem_szama + sajat_tomeg + teljes_tomeg" 
                         ),
                         knn_impute, .data = cars_data
) %>% 
  bind_cols()

cars_data_imputed <- cars_data %>% 
  select(-names(imputed_df)) %>% 
  bind_cols(imputed_df)

last_prices_unsold_df <- last_price_df %>% 
  filter(end >= "2022-03-01", start < "2022-03-01") # already seen in February but unsold

cars_unsold_df <- last_prices_unsold_df %>% 
  select(id) %>% 
  left_join(cars_data_imputed)

accessories_unsold_df <- last_prices_unsold_df %>% 
  select(id) %>% 
  left_join(accessories_df)

prices_unsold_df <- last_prices_unsold_df %>% 
  select(id) %>% 
  left_join(prices_df)


save(cars_data_imputed, last_prices_unsold_df, cars_unsold_df, accessories_unsold_df, prices_unsold_df, file = "data/unsold_cars.RData") # TODO need cars_data_imputed?

