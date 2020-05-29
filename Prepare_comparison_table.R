# This script is used to generate a list of regional categories which is used to make a blank table for grades.
# So it was run only once and, as a general rule, is not intended to be re-run (except for the case when you've updated Таблица_ООПТ.ods and want to update the whole research).

# Load libraries
library(dplyr)
library(readODS)

# Read table with a list of all regional categories
list <- read_ods("../../ООПТ по ФО/Таблица ООПТ.ods", skip = 1)

# We assume that categories with the same name in different regions are different categories, but categories with the same name inside one region (that is, one of regional importacne and one of local importance) are the same category.
# So we need to aggregate the table so that each category inside a region is represented by only one row.
# That we need to save table to a separate file (filename is with timestamp to avoid overwriting)
list %>% 
  select(number, district, region, category_name) %>% 
  group_by(region) %>% 
  distinct(category_name, .keep_all = TRUE) %>% 
  ungroup() %>% 
  write_ods(paste0("Comparison_table_raw_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".ods"))
  
