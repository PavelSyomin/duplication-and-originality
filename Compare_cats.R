# Load libraries
library(readODS)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Load three grades tables made by authors, calculate a level of agreement between assessors, and prepare a summary table of grades
source("Calculate_agreement_and_aggregate_grades.R")

# Load table of tasks of federal categories
source("Fed_cats_tasks_table.R")

# Load table of regional categories with their levels
source("Prepare_list_of_regional_cats_with_levels.R")

# Read data from a table of aggregated scores (the table is created in Calculate_agreement_and_aggredate_grades.R)
d <- read.csv("Comparison_table_raw.csv", stringsAsFactors = FALSE)
# Rebuild the numbers of regional categories
d$number <- 1:nrow(d)
# Remove undefined categories ("не_определено_*")
d <- d[!grepl("не_определено_\\d{1,3}", d$category_name), ]
# Find out the total number of rows
nrow_total <- nrow(d)
# Remove missing rows
data <- na.exclude(d)
# Find out the number ofrows with estimates
nrow_filled <- nrow(data)
# Find out the number of regional categories where it hasn't been possible to make the estimates
rcats_nodata <- nrow_total - nrow_filled

# Function to calculate the index of similarity
# Used in apply
# Index is between 0 and 1, where 1 is a maximum similarity
# Gets a data frame of grades and a reference table of grades
# Returns a vector
calculate_similarity <- function(x, fcats_tasks_table) {
  # Store id of a regional category
  rcat_number <- as.integer(x[1])
  # Store grades of objectives
  rcat_objectives <- as.integer(x[-1])
  # Compare vector of objectives with each row in a reference table
  raw_result <- apply(fcats_tasks_table, 1, function(y) {
    # Store id and name of a reference category
    catid <- as.integer(y[1])
    cat_name <- y[2]
    # Store grades of objectives of a federal category
    fcat_objectives <- as.integer(y[-c(1:2)])
    # Calculate difference between grades
    diff <- abs(rcat_objectives - fcat_objectives)
    # Count a number of objectives with matched grades
    full_matches <- diff[diff == 0]
    # Count a number of objectives with a difference of 1
    partial_matches <- diff[diff == 1]
    # Calculate total grade of similarity: 2 balls for a full match and 1 ball for a partial match
    grade <- length(full_matches) * 2 + length(partial_matches)
    # Convert balls to a similarity index
    grade <- round(grade / 18, digits = 2) # 18 is max grade (2 balls * 9 objectives)
    # Return a vector with catid and grade
    c(catid = catid, grade = grade)
  })
  # Rotate a matrix and turn it into a data frame
  raw_result <- as.data.frame(t(raw_result))
  # We have variants of zakazniks and natural monuments. Variants will result in a different similarity index, but we shouldn't process all variants as different categories. So we find the maximum similarity among variants. For the other four categories we have only one variant, so the code just returns its value
  result <- aggregate(grade ~ catid, raw_result,  FUN = max)
  # Make a vector and return it
  c(rcat_number, result$grade)
}

# Process data
x <- as.data.frame(t(apply(data[-c(2:4)], 1, calculate_similarity, fcats_tasks_table)))
colnames(x) <- c("number", "zap", "np", "pp", "zak", "mon", "dpbg")
# Add categories' names, regions and federal districts by joining with initial datarfame using numbers of regional caterories
comparison_results <- merge(data[, 1:4], x, by = "number")
# Find out which federal category has the maximum similarity with each regional category
comparison_results[, c("max", "zap_is_max", "np_is_max", "pp_is_max", "zak_is_max", "mon_is_max", "dpbg_is_max", "none_is_max", "max_fcat_names")] <- t(apply(comparison_results, 1, function(x) {
  # Store similarity indices only
  x <- x[-c(1:4)]
  # Find max similarity index
  max <- max(x)
  # Store codes for the names of federal categories
  fcat_names <- c("zap", "np", "pp", "zak", "mon", "dpbg")
  # Set a threshold
  threshold = .7
  # In index is higher than the threshold, set 1 for the respective federal category (or categories) and store its name (or names)
  if (max >= threshold) {
    # Is the given federal category has the maximum similarity index?
    fcat_is_max <- c(as.integer(x == max), 0)
    # Get a string with codes of respective federal categories separated by “+” and store it
    fcat_max_names <- paste0(fcat_names[x == max], collapse = "+")
  }
  # Else the category is “original”
  else {
    fcat_is_max <- c(rep(0, 6), 1)
    fcat_max_names <- "none"
  }
  # Return resulting vector
  # E. g. ([Max similarity index] 0.67, [Is it with zap? Yes] 1, [Is it with np? No] 0, [With pp? No] 0, [With zak? No] 0, [With mon? No] 0, [With dpbg? No] 0, [Is it original category? No] 0, [Federal category with maximum similarity] "zap"). Of course, return is without text — only codes and digits
  c(max, fcat_is_max, fcat_max_names)
}))

# Fix column types, because we had to coerce everything to a character vector on the previous step to return 0 (numeric) in a one vector with "zap" (character)
comparison_results$max <- as.numeric(comparison_results$max)
comparison_results[, c("zap_is_max", "np_is_max", "pp_is_max", "zak_is_max", "mon_is_max", "dpbg_is_max", "none_is_max")] <- apply(comparison_results[, c("zap_is_max", "np_is_max", "pp_is_max", "zak_is_max", "mon_is_max", "dpbg_is_max", "none_is_max")], 2, as.integer)

# Join data frame with regional categories' levels and a current data frame
comparison_results <- merge(comparison_results, rcats_with_levels, by = c("number", "district", "region", "category_name"), all.x = TRUE)
# Set the correct order
comparison_results <- comparison_results[order(comparison_results$number),]

# Summary table
matched_fcats <- table(comparison_results$max_fcat_name)
# Order rows by number of matched regional categories descending
# Turn into a data frame
matched_fcats_df <- as.data.frame(matched_fcats[order(matched_fcats, decreasing = TRUE)])
# Add sensible colnames
colnames(matched_fcats_df) <- c("fcat", "n_rcats")
# Turn codes to names, because this data frame is to be displayed in an article
matched_fcats_df$fcat <- str_replace_all(matched_fcats_df$fcat,
                                         c("zap" = "заповедники",
                                           "np" = "национальные парки",
                                           "pp" = "природные парки",
                                           "zak" = "заказники",
                                           "mon" = "памятники природы",
                                           "dpbg" = "дендрологические парки и ботанические сады",
                                           "none" = "отсутствует"
                                           ))
# Replace “+” with a comma
matched_fcats_df$fcat <- gsub("+", ", ", matched_fcats_df$fcat, fixed = TRUE)
# Capitalise the first letter in each row
matched_fcats_df$fcat <- Hmisc::capitalize(matched_fcats_df$fcat)

# Final table shows the number of duplicated categories with respect not only to the analogous federal category, but also to the level of regional category
final_table <- comparison_results %>% 
  group_by(category_level) %>% 
  summarise_at(vars(zap_is_max:none_is_max), sum) %>% 
  bind_rows(
    summarise_all(., sum)
  ) %>% 
  mutate(total = rowSums(.[-1], na.rm = TRUE),
         category_level = factor(category_level,
                                 levels = c(1, 2, 3, 6),
                                 labels = c("Только региональное",
                                            "Только местное",
                                            "Региональное или местное",
                                            "Сумма")))
  


# Compare within-group and between-group variability in indices
# Group 1: federal categories
# Group 2: regional categories
# Can we say that within-group similarity of categories (especially federal ones) is smaller than between-groups similatiry?
# If true, this is a strong point to the conclusion that regional categories are in fact more similar to federal categories that they should be

# Comrate federal categories with federal categories in the same way we have compared regional categories with federal categories
fcats_fcats_comparison <- as.data.frame(t(apply(fcats_tasks_table[-2], 1, calculate_similarity, fcats_tasks_table)))
# Change colnames to something more human-readible
colnames(fcats_fcats_comparison) <- c("number", "zap", "np", "pp", "zak", "mon", "dpbg")
# Combine all indices to a vector
fcats_fcats_indices <- do.call(c, fcats_fcats_comparison[-1])
# Remove indices that are equal to 1, because these indices are a result of comparison of a category with itself
fcats_fcats_indices <-  fcats_fcats_indices[fcats_fcats_indices != 1]
# A simple plot
qplot(fcats_fcats_indices, geom = "bar", xlim = c(0, 1))
# Check for the normality of distribution
shapiro.test(fcats_fcats_indices)
# Get descriptive statistics
psych::describe(fcats_fcats_indices)
# Find out the most similar category in each row (i. e. the most similar federal category for each variant of the federal categories)
fcats_fcats_comparison["max"] <- apply(fcats_fcats_comparison, 1, function(x) {
  x <- x[-1]
  x <- x[x != 1]
  max(x)
})
# Get descriptive statistics
psych::describe(fcats_fcats_comparison["max"])

# Do the same for the similarity indices between federal an regional categories
# We already have a data frame with indeces, so just put them to a vector
# We don't need to exclude indices that equals 1, because here they're not the result of comparison of the category with itself
rcats_fcats_indices <- do.call(c, select(comparison_results, zap:dpbg))
# Normality test
shapiro.test(rcats_fcats_indices)
# Descriptive stats (for all indices and for maximum indices)
psych::describe(rcats_fcats_indices)
psych::describe(comparison_results["max"])

# Mann-Whitney test for the two groups of indices
ff_rf_indices_diff <- wilcox.test(fcats_fcats_indices, rcats_fcats_indices, conf.int = TRUE)

# A plot of distributions of indices
# We use geom_bar, because it's simplier to show proportions on it (geom_density also would be cool, but it will be far more difficult for a reader to understand what does “density” mean)
ff_rf_indices_plot <- rbind(
  data.frame(variant = "ff", indices = fcats_fcats_indices),
  data.frame(variant = "rf", indices = rcats_fcats_indices)
) %>% 
ggplot(aes(x = indices, fill = variant)) +
  geom_bar(aes(y = stat(prop)), color = "black", position = position_dodge2(preserve = "single", padding = .3)) +
  scale_fill_manual(name = "", labels = c("Внутри федеральных категорий", "Между федеральными и региональными категориями"), values = c(ff = "black", rf = "white")) +
  scale_x_continuous(labels = function(x) format(x, decimal.mark = ","), limits = c(0, 1)) +
  scale_y_continuous(labels = function(x) format(x, decimal.mark = ","), expand = expand_scale(mult = c(0, .1))) +
  labs(x = "Индекс сходства", y = "Доля индексов") +
  theme_bw(base_family = "PT Sans") +
  theme(legend.direction = "vertical", legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.text = element_text(size = 14), panel.border = element_blank())
# Display the plot
ff_rf_indices_plot
