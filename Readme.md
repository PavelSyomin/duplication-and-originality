# Data and code for an article comparing federal and regional categories of protected areas in Russia

The article is an unpublished draft.

The article is written using R and RMarkdown. The R source code is split into several files. Some of them are intended to be run only once, because they create templates, while the others process the data and provide for an “assembly line” which builds the article. Article.Rmd is the main file. It loads all necessary dependencies, so you can knit it and get the full article with a single click.

The raw normativa data for assessment of regional categories is in the **npa** folder. The results of an assessment made by the three co-authors are in *Comparison_table_raw_{suffix}.ods*.