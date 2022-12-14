getwd() %>% 
  str_replace("used-car-liquidity", "diesel-cars/data") %>% 
  list.files(full.names = TRUE) %>% 
  walk(~ {
    new_file <- gsub(".*data/", "data/", .)
    print(new_file)
    file.copy(., new_file)
  })