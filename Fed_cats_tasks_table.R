# We have original table from (Благовидов, Очагов, Птичников. Сохранение биоразнообразия лесов России: вклад ООПТ и лесов I группы. М., 2002. С. 26)
# The table has been edited a bit; see Table_of_tasks_of_federal_categories.ods

# The problem: some objectives for zakazkiks and nature monuments have more than 1 estimate
# E. g. “nature protection” for zakazniks may have an estimate of 1, 2, or 3
# But for further analysis we need a table of integers with exactly 1 digit in a cell
# The solution is to prepare table with all possible combinations
# So zapovedniks, national and nature parks, as well as dendrological parks and botanical gardens will have only 1 row
# While zakazkiks will have 54 rows, and nature monuments will have 3 rows


# The simpliest part: table of tasks for zapovedniks, national parks, nature parks and dendrological parks with botanical gardens
four_cats <- data.frame(catid = c(1:3, 6),
                cat_name = c("zap", "np", "pp", "dpbg"),
                sci = c(1, 2, 3, 2),
                wild = c(2, 3, 3, 4),
                div = c(1, 2, 3, 1), 
                serv = c(1, 1, 1, 4),
                feat = c(3, 1, 3, 4),
                tour = c(2, 1, 1, 3),
                edu = c(1, 1, 3, 2),
                use = c(4, 3, 4, 4),
                cult = c(4, 3, 4, 4))

# Save colnames to a vector to avoid repetition
column_names <- c("catid", "cat_name", "sci", "wild", "div",
                  "serv", "feat", "tour", "edu", "use", "cult")

# Variants of zakazniks
zak <- expand.grid(4, "zak", 3, 1:3, 1:3, 1:3, 3, 3, 3, 2:3, 3)
colnames(zak) <- column_names

# Varinats of nature monuments
mon <- expand.grid(5, "mon", 4, 3, 1:3, 4, 1, 3, 3, 4, 4)
colnames(mon) <- column_names

# Combine everything to 1 table
fcats_tasks_table <- rbind(four_cats, zak, mon)
fcats_tasks_table <- fcats_tasks_table[order(fcats_tasks_table$catid), ]
