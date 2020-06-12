# Load libraries
library(dplyr)
library(readODS)

# Load table with regional categories
rcats_list <- read_ods("Regional_categories.ods", skip = 1)

# Prepare a list of regional categories with their levels
# Initial table has category_level field with two possible values: 1 (regional) and 2(local)
# If a regional category may have both regional and local level, than there are two rows in a table
# So we may use sum to indicate whether a regional category is of regional level only (sum = 1), of local level only (sum = 2), or both of regional and local level (sum = 3)
rcats_with_levels <- rcats_list %>% 
  select(number, district, region, category_name, category_level) %>% 
  group_by(region, category_name) %>%
  summarise(number = first(number), district = first(district), category_level = sum(category_level)) %>% 
  select(number, district, region, category_name, category_level) %>% 
  arrange(number) %>% 
  ungroup() %>% 
  mutate(number = row_number())