# Load libraries
# To read data from OpenDocument spreadsheets
library(readODS)
# To calculate Fleiss' Kappa
library(irr)
# To simplify processing
library(dplyr)
# To make plots
library(ggplot2)

# Read tables with grades by each assessor
t1 <- read_ods("Comparison_table_raw_a1.ods", sheet = 2) # Author 1
t2 <- read_ods("Comparison_table_raw_a2.ods", sheet = 2) # Author 2
t3 <- read_ods("Comparison_table_raw_a3.ods", sheet = 2) # Author 3

# Rebuild row numbers
t1$number <- 1:nrow(t1)
t2$number <- 1:nrow(t2)
t3$number <- 1:nrow(t3)

# Check if we have only two options: the row is fully filled with grades or the row is fully empty
t1 %>% 
  select(sci:cult) %>% 
  apply(., 1, function(x) sum(is.na(x))) %>% 
  sum(.) %% 9 == 0 # if TRUE, everything is OK

t2 %>% 
  select(sci:cult) %>% 
  apply(., 1, function(x) sum(is.na(x))) %>% 
  sum(.) %% 9 == 0

t3 %>% 
  select(sci:cult) %>% 
  apply(., 1, function(x) sum(is.na(x))) %>% 
  sum(.) %% 9 == 0

# Join all grades for each assessor to a single numeric vector
s1 <- do.call(c, t1[-c(1:4)])
s2 <- do.call(c, t2[-c(1:4)])
s3 <- do.call(c, t3[-c(1:4)])

# Put vectors to a data frame
grades <- data.frame(p = s1, j = s2, l = s3)
# Replace NAs with zeros, because kappam.fleiss skip NAs, but we need to account for NAs when computing a measure of agreement
grades[is.na.data.frame(grades)] <- 0
# Calculate Fleiss' Kappa
kappa <- kappam.fleiss(grades)
# Print kappa
kappa

# Pairwise Cohen's kappa
kappa2(grades[1:2]) # Authors 1 and 2
kappa2(grades[2:3]) # Authors 2 and 3
kappa2(grades[c(1,3)]) # Authors 1 and 3

# A bit more detailed comparison: check the distribution of differences between min and max grades
# E. g. if the three grades by assessors are (1, 2, 4), the distance is (4-1) = 3
# Firstly, calculate the distance between min and max
grades_distance <- apply(grades, 1, function(x) {
  x <- x[x != 0]
  if (length(x) > 0) max(x) - min(x) else NA
})

# Secondly, draw a plot
data.frame(dist = grades_distance) %>% 
  na.omit(.) %>% 
  ggplot(aes(x = dist)) +
  geom_bar(color = "black", fill = "white") +
  geom_text(aes(label = stat(count)), stat = "count", position = position_nudge(y = 50)) +
  geom_text(aes(label = format(stat(prop), digits = 1)), stat = "count", position = position_nudge(y = -50))

# A distribution of grades made by each assessor
# Just for fun, because I can't imagine what to do with these plots
qplot(x = s1, geom = "bar", xlab = "Grades by Author 1")
qplot(x = s2, geom = "bar", xlab = "Grades by Author 2")
qplot(x = s3, geom = "bar", xlab = "Grades by Author 3")

# Which number of categories was assessed by one, two or three assessors?
apply(grades, 1, function(x) {
  names(x) <- c("a1", "a2", "a3")
  x <- x[x != 0]
  paste0(names(x), collapse = "")
}) %>% table(.)

# Build a table with aggregated grades
# Add column to mark assessor (this is possibly an extra useless step, but let it be)
t1$acessor <-  "a1"
t2$acessor <- "a2"
t3$acessor <- "a3"

# Join three tables with grades into a one big table
t <- rbind(t1, t2, t3)

# Aggregate grades using the following rule:
# if two or three assessors decided that it is not possible to set a grade to a category (that is, NAs in a row), we set the aggregated grade to NA
# if two or three assessors have managed to grade a category, we use a median (NA, if there is one, is skipped) as an aggregated grade. We also drop the decimal part of the median if necessary (e. g. grades are (NA, 2, 3), and median is 2.5, so we need to floor the median to avoid non-integer values)
r <- t %>% 
  group_by(number, district, region, category_name) %>% 
  summarise_at(vars(sci:cult), function(x) {
    if (sum(is.na(x)) >= 2) return(NA)
    else floor(median(x, na.rm = TRUE))
    })

# Save aggregated table to Comparison_table_raw.csv (we'll use it for analysis)
write.csv(r, "Comparison_table_raw.csv", row.names = FALSE)

# Test if the code works correctly (if it works, r must equals t1 if all three initial tables are with the same content)
# sum(r != t1[-14], na.rm = TRUE)
