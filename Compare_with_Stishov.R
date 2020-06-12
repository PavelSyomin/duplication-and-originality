# Compare our results with [Stishov, Dudley, 2018]
# This script is just a helper to facilitate a manual search
# It read the table with all categories from [Stishov, Dudley, 2018] listed in a column and tries to find the category with the same name in comparison_results

# We need to source Compare_cats.R to get comparison_results if this data frame has not already been loaded in the environment
source("Compare_cats.R")

# Read table with names of regional categories according to Stishov and find categories with the same name in our table
# Then count the numbers of analogous federal categories
stishov <- read_ods("Compare_Stishov_with_our_results.ods")   
lapply(stishov$category_name, function(x) {
  filtered <- comparison_results[comparison_results$category_name == x,]
  filtered %>% 
    summarise_at(vars(zap_is_max:dpbg_is_max), sum) %>% 
    mutate(cat_name = x) %>% 
    select(cat_name, zap_is_max:dpbg_is_max)
})

# If we didn't found category automatically (e. g. because of difference in the way the names were written), we can try to grep them manually using this expression
# Place the search in double quotes "" e. g. grep("парк", …)
comparison_results[grep("", comparison_results$category_name, fixed = TRUE),]
