# Load libraries
library(readODS)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Load three grades tables made by authors, calculate a level of agreement between assessors, and prepare a summary table of grades
source("Calculate_agreement.R")

# Load table of tasks of federal categories
source("Fed_cats_tasks_table.R")

# Load table of federal categories with their levels
source("Prepare_list_of_federal_cats_with_levels.R")

# Read data from ods
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
calculate_similarity <- function(x, table) {
  rcat_number <- as.integer(x[1])
  rcat_objectives <- as.integer(x[-1])
  raw_result <- apply(table, 1, function(y) {
    catid <- as.integer(y[1])
    cat_name <- y[2]
    fcat_objectives <- as.integer(y[-c(1:2)])
    diff <- abs(rcat_objectives - fcat_objectives)
    full_matches <- diff[diff == 0]
    partial_matches <- diff[diff == 1]
    grade <- length(full_matches) * 2 + length(partial_matches)
    grade <- round(grade / 18, digits = 2) # 18 is max grade (2 balls * 9 objectives)
    c(catid = catid, grade = grade)
  })
  raw_result <- as.data.frame(t(raw_result))
  result <- aggregate(grade ~ catid, raw_result,  FUN = max)
  c(rcat_number, result$grade)
}

# Process data
x <- as.data.frame(t(apply(data[-c(2:4)], 1, calculate_similarity, table)))
colnames(x) <- c("number", "zap", "np", "pp", "zak", "mon", "dpbg")
# Add categories' names, regions and federal districts by joining with initial datarfame using numbers of regional caterories
comparison_results <- merge(data[, 1:4], x, by = "number")
# Find out which federal category has the maximum similarity with each regional category
comparison_results[, c("max", "zap_is_max", "np_is_max", "pp_is_max", "zak_is_max", "mon_is_max", "dpbg_is_max", "none_is_max", "max_fcat_names")] <- t(apply(comparison_results, 1, function(x) {
  x <- x[-c(1:4)]
  max <- max(x)
  fcat_names <- c("zap", "np", "pp", "zak", "mon", "dpbg")
  # In index is lower than 0.7, there is no similar federal category, and the regional category is treated as “original”
  if (max >= .65) {
    fcat_is_max <- c(as.integer(x == max), 0)
    fcat_max_names <- paste0(fcat_names[x == max], collapse = "+")
  }
  else {
    fcat_is_max <- c(rep(0, 6), 1)
    fcat_max_names <- "none"
  }
  c(max, fcat_is_max, fcat_max_names)
}))

# Fix column types
comparison_results$max <- as.numeric(comparison_results$max)
comparison_results[, c("zap_is_max", "np_is_max", "pp_is_max", "zak_is_max", "mon_is_max", "dpbg_is_max", "none_is_max")] <- apply(comparison_results[, c("zap_is_max", "np_is_max", "pp_is_max", "zak_is_max", "mon_is_max", "dpbg_is_max", "none_is_max")], 2, as.integer)
# comparison_results$max_fcat_name <- factor(comparison_results$max_fcatid,
                                           # levels = 0:6,
                                           # labels = c("none", "zap", "np", "pp", "zak", "mon", "dpbg"))

# Join data frame with regional categories' levels and a current data frame
comparison_results <- merge(comparison_results, rcats_with_levels, by = c("number", "district", "region", "category_name"), all.x = TRUE)
comparison_results <- comparison_results[order(comparison_results$number),]

# Summary table
matched_fcats <- table(comparison_results$max_fcat_name)
# Order rows by number of matched regional categories descending
# Turn into a data frame
matched_fcats_df <- as.data.frame(matched_fcats[order(matched_fcats, decreasing = TRUE)])
# Add sensible colnames
colnames(matched_fcats_df) <- c("fcat", "n_rcats")
matched_fcats_df$fcat <- str_replace_all(matched_fcats_df$fcat,
                                         c("zap" = "заповедники",
                                           "np" = "национальные парки",
                                           "pp" = "природные парки",
                                           "zak" = "заказники",
                                           "mon" = "памятники природы",
                                           "dpbg" = "дендрологические парки и ботанические сады",
                                           "none" = "отсутствует"
                                           ))
matched_fcats_df$fcat <- gsub("+", ", ", matched_fcats_df$fcat, fixed = TRUE)
matched_fcats_df$fcat <- Hmisc::capitalize(matched_fcats_df$fcat)

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
  

comparison_results %>% 
  summarise_at(vars(zap_is_max:none_is_max), sum)

indices_desc_stats <- comparison_results %>% 
  select(zap:dpbg) %>% 
  apply(., 2, function(x) {
    min_index = min(x)
    first_quartile = quantile(x, .25, names = FALSE)
    median_index = median(x)
    mean_index = mean(x)
    third_quartile = quantile(x, .75, names = FALSE)
    max_index = max(x)
    index_iqr = IQR(x)
    index_normality = shapiro.test(x)$p.value
    c(min = min_index, first_q = first_quartile, median = median_index, mean = mean_index, third_q = third_quartile, max = max_index, iqr = index_iqr, norm_p = index_normality)
  }) %>% 
  as.data.frame()

rownames(indices_desc_stats) <- c(
  "Минимум",
  "Первый квартиль",
  "Медиана",
  "Среднее",
  "Третий квартиль",
  "Максимум",
  "Межквартильный размах",
  "Sharipo-Wilk p-value"
)

indices_desc_stats %>% slice(1:4)

indices_distr_plot <- comparison_results %>% 
  gather(fcat, index, zap:dpbg) %>% 
  ggplot(aes(x = index)) +
  geom_histogram(bins = 18) +
  geom_density(color = "blue") +
  geom_vline(xintercept = .72, color = "red") +
  facet_wrap(~fcat)


# Compare within-group and between-group variability in indices
# Group 1: federal categories
# Group 2: regional categories
# Can we say that within-group similarity of categories (especially federal ones) is smaller than between-groups similatiry?
# If true, this is a strong point to the conclusion that regional categories are in fact more similar to federal categories that they should be
fcats_fcats_comparison <- as.data.frame(t(apply(table[-2], 1, calculate_similarity, table)))
colnames(fcats_fcats_comparison) <- c("number", "zap", "np", "pp", "zak", "mon", "dpbg")
fcats_fcats_indices <- do.call(c, fcats_fcats_comparison[-1])
fcats_fcats_indices <-  fcats_fcats_indices[fcats_fcats_indices != 1]
qplot(fcats_fcats_indices, geom = "bar", xlim = c(0, 1))
min(fcats_fcats_indices)
quantile(fcats_fcats_indices, c(.25, .5, .75))
max(fcats_fcats_indices)
mean(fcats_fcats_indices)
shapiro.test(fcats_fcats_indices)
psych::describe(fcats_fcats_indices)
fcats_fcats_comparison["max"] <- apply(fcats_fcats_comparison, 1, function(x) {
  x <- x[-1]
  x <- x[x != 1]
  max(x)
})
psych::describe(fcats_fcats_comparison["max"])

rcats_fcats_indices <- do.call(c, select(comparison_results, zap:dpbg))
# rcats_fcats_indices <- rcats_fcats_indices[rcats_fcats_indices != 1]
qplot(rcats_fcats_indices, geom = "bar", xlim = c(0,1))
min(rcats_fcats_indices)
quantile(rcats_fcats_indices, c(.25, .5, .75))
max(rcats_fcats_indices)
mean(rcats_fcats_indices)
shapiro.test(rcats_fcats_indices)
psych::describe(rcats_fcats_indices)
psych::describe(comparison_results[comparison_results$max != 1, "max"])

ff_rf_indices_diff <- wilcox.test(fcats_fcats_indices, rcats_fcats_indices, conf.int = TRUE)

rcats_rcats_comparison <- as.data.frame(t(apply(data[-c(2:4)], 1, calculate_similarity, data[-c(3,4)])))
rcats_rcats_indices <- do.call(c, rcats_rcats_comparison[-1])
rcats_rcats_indices <- rcats_rcats_indices[rcats_rcats_indices != 1]
min(rcats_rcats_indices)
quantile(rcats_rcats_indices, c(.25, .5, .75))
max(rcats_rcats_indices)
mean(rcats_rcats_indices)
# shapiro.test(rcats_rcats_indices) # won't work cause to a sample size limit set to 5000
qplot(rcats_rcats_indices, geom="bar", xlim = c(0,1))
rcats_rcats_comparison["max"] <- apply(rcats_rcats_comparison, 1, function(x) {
  x <- x[-1]
  x <- x[x != 1]
  max(x)
})
psych::describe(rcats_rcats_comparison["max"])


ff_rf_indices_plot <- rbind(
  data.frame(variant = "ff", indices = fcats_fcats_indices),
  data.frame(variant = "rf", indices = rcats_fcats_indices)
) %>% 
ggplot(aes(x = indices, color = variant)) +
  geom_density() +
  geom_density() +
  scale_color_manual(name = "", labels = c("Внутри федеральных", "Между федеральными и региональными"), values = c(ff = "blue", rf = "red")) +
  labs(x = "Значение индекса сходства", y = "Плотность") +
  theme(legend.position = "bottom")
