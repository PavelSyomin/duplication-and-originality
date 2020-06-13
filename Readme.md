# Data and code for an article comparing federal and regional categories of specially protected natural territories in Russia

The article is an unpublished draft.

The article is written using R and RMarkdown. The R source code is split into several files. Some of them are intended to be run only once, because they create templates, while the others process the data and provide for an “assembly line” which builds the article. Article.Rmd is the main file. It loads all necessary dependencies, so you can knit it and get the full article with a single click.

## A detailed description of files

* **npa** — a folder with a raw normative data for assessment of regional categories;

* **.Rprofile** — a file that helps to load necessary libraries right when the project is open;

* **Article.Rmd** — the **main file** of the article;

* **Calculate_agreement_and_aggregate_grades.R** — a script which loads three tables with raw grades (see further), calculates Fleiss' kappa, aggregates three grades to a one grade and writes the result in a csv file. Required by Compare_cats.R;

* **Compare_Stishov_with_our_results.ods** — a table with a comparison of our results with results from M. S. Stishov and N. Dudley's book (Стишов М. С., Дадли Н. Охраняемые природные территории Российской Федерации и их категории. М.: WWF, 2018);

* **Compare_cats.R** — a script which performs the comparison and builds tables and a plot used in the article, i. e. the main script. “Cats” means categories, not cats :-)

* **Compare_with_Stishov.R** — a helper script used to prepare a *Compare_Stishov_with_our_results.ods* table. In fact, it just searches for categories' names in a data frame with our results;

* **Comparison_table_raw_{suffix}.ods** — 
the results of an assessment made by the three co-authors.

* **Create_odt_for_reference_styles.Rmd** — an RMarkdown document used to create ad ODT file by Pandoc and then customize some styles. The generated and customized file is saved as *reference-styles.odt* (see further);

* **Fed_cats_tasks_table.R** — a script to build a table of tasks of federal categories;

* **Misc.R** — parts of the code cut from Compare_cats.R, which we didn't want to delete totally. If one runs this file, he will probably see something interesting, but in general, this file is not very useful, and the analysis here wasn't included in the final version of the article;

* **Prepare_comparison_table.R** — a helper script which was used to generate the ODS to fill in with grades;

* **Prepare_list_of_regional_cats_with_levels.R** — load a list of regional categories with their levels;

* **Readme.md** — this readme;

* **Table_of_tasks_of_federal_categories.ods** — a short version of table, which is loaded in the *Article.Rmd* and displayed as a part of “Materials and Methods”;

* **jrp.csl** — a citation style file for Journal of Russian Law (a modified version of GOST R 7.05-2008 CSL);

* **rcats_references.bib** — bibliography in BibTeX;

* **reference-styles.odt** — an ODT with reference styles to knit the article.

## The Knitting process

When a user opens Article.Rmd and clicks “Knit” button, the Compare_cats.R is sourced. Compare_cats.R itself sources three files: Calculate_agreement_and_aggregate_grades.R, Fed_cats_tasks_table.R, and Prepare_list_of_regional_cats_with_levels.R. The first of these files loads three ODS tables with raw comparison grades (Comparison_table_raw_{suffix}), does necessary aggregations and writes the result to Comparison_table_raw.csv. The second file prepares a data frame with by-objective grades of federal categories. The third file prepares a data frame with regional categories' names and levels. After these scripts are executed, the rest of Compare_cats.R is processed, and all comparison is made. When Compare_cats.R is processed, the Article.Rmd is knit to Article.odt.