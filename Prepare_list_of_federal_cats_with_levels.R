library(dplyr)
library(readODS)

list <- read_ods("../../ООПТ по ФО/Таблица ООПТ.ods", skip = 1)

rcats_with_levels <- list %>% 
  select(number, district, region, category_name, category_level) %>% 
  group_by(region, category_name) %>%
  summarise(number = first(number), district = first(district), category_level = sum(category_level)) %>% 
  select(number, district, region, category_name, category_level) %>% 
  arrange(number) %>% 
  ungroup() %>% 
  mutate(number = row_number())