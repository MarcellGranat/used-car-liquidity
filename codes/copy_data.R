# copy the raw data from another local repository
# securing that we download daily the data and we do not touch that repo by mistake
# you may also copy that by hand

getwd() %>% 
  str_replace("used-car-liquidity", "hasznaltauto/data") %>% 
  list.files(full.names = TRUE) %>% 
  walk(~ {
    new_file <- gsub(".*data/", "data/", .)
    print(new_file)
    file.copy(., new_file)
  })