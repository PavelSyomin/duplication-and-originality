# Miscellanious code that isn't necessary to compile an article but does some staff which may be useful in some circumstances
# In fact, it was written as a part of Compare_cats.R and had something to do with draft analysis or testing or interpretation of the results
# I didn't want to remove it, so I cut it from Compare_cats.R and saved here

# The code requires objects which are created in Compare_cats.R, so source this file
source("Compare_cats.R")

# By-column descriptive statistics of comparison_results
# It describes indices which resulted from the comparison of regional categories with each federal category, and so describes how similar is the given federal category to the regional categories
# We may say that it is a descriptive statistics not for regional categories, but for federal categories
# Originally it was included in article text, but further it was removed due to article's size limit and also because it was useless for discussion
# Get descriptive stats
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

# Add rownames
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

# A slice of descriptive stats
indices_desc_stats %>% slice(1:4)

# A plot with distribution of indices facetted by each federal category
indices_distr_plot <- comparison_results %>% 
  gather(fcat, index, zap:dpbg) %>% 
  ggplot(aes(x = index)) +
  geom_histogram(bins = 18) +
  geom_density(color = "blue") +
  geom_vline(xintercept = .72, color = "red") +
  facet_wrap(~fcat)

# A comparison of regional caregories with regional categories
# Doesn't used in the article, and, franlky speaking, it's hard to explain when does this comparison may be useful, but, all in all, we can do it and see the results (which are quiet interesting)
# Make a comparison
rcats_rcats_comparison <- as.data.frame(t(apply(data[-c(2:4)], 1, calculate_similarity, data[-c(3,4)])))
# Coerce everything to one vector
rcats_rcats_indices <- do.call(c, rcats_rcats_comparison[-1])
# Remove indices that are equal to 1 because this is the result of comparison of a category with itself (sometimes it is not, but such cases are rare)
rcats_rcats_indices <- rcats_rcats_indices[rcats_rcats_indices != 1]
# shapiro.test(rcats_rcats_indices) # won't work cause to a sample size limit set to 5000
# A simple plot
qplot(rcats_rcats_indices, geom="bar", xlim = c(0,1))
# Find out maximum of indices for each regional category
rcats_rcats_comparison["max"] <- apply(rcats_rcats_comparison, 1, function(x) {
  x <- x[-1]
  x <- x[x != 1]
  max(x)
})
# Show descriptive statistics
psych::describe(rcats_rcats_indices)
psych::describe(rcats_rcats_comparison["max"])
