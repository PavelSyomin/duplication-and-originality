# Description | Описание

## [en] Data and code for an article comparing federal and regional categories of specially protected natural territories in Russia

By Pavel Syomin (Habr.com), Julia Kiel and Valeriya Sinitsina (Ural State Law University).

The article is publised in “Antinomii” journal (no. 4, 2020) and can be found at https://doi.org/10.24411/2686-7206-2020-10408.

The article is written using R and RMarkdown. The R source code is split into several files. Some of them are intended to be run only once, because they create templates, while the others process the data and provide for an “assembly line” which builds the article. **Article.Rmd** is the main file. It loads all necessary dependencies, so you can knit it and get the full article with a single click.

### A detailed description of the files 

* **npa** — a folder with a raw legal data for the assessment of regional categories;

* **.Rprofile** — a file that helps to load necessary libraries right when on the project startup;

* **Article.Rmd** — the **main file** of the article;

* **Calculate_agreement_and_aggregate_grades.R** — a script which loads three tables with raw grades (see further), calculates Fleiss' kappa, aggregates three grades into a one grade and writes the result in a csv file. Required by Compare_cats.R;

* **Cats_similarity.Rproj** — a project file;

* **Compare_Stishov_with_our_results.ods** — a table with a comparison of our results with results from M. S. Stishov and N. Dudley's book (Стишов М. С., Дадли Н. Охраняемые природные территории Российской Федерации и их категории. М.: WWF, 2018);

* **Compare_cats.R** — a script which performs the comparison and builds tables and a plot used in the article, i. e. the main script. “Cats” means categories, not cats :-)

* **Compare_with_Stishov.R** — a helper script used to prepare a *Compare_Stishov_with_our_results.ods* table. In fact, it just searches for categories' names in a data frame with our results;

* **Comparison_results.csv** — a csv with the similarity indices and the most similar federal categories for each regional categories (except for the regional categories we were unable to access due to the lack of information). It is auto-generated by Compare_cats.R on each run, but if one doesn't want to run the full script, but still wants to see the detailed comparison results, he may refer to this file;

* **Comparison_table_raw_{suffix}.ods** — 
the results of an assessment made by three co-authors. {suffix} stands for the name of the author: p — **P**avel S., j — **J**ulia K., l — Va**l**eriya S.;

* **Create_odt_for_reference_styles.Rmd** — an RMarkdown document used to create ad ODT file by Pandoc and then customize some styles. The generated and customized file is saved as *reference-styles.odt* (see further);

* **Fed_cats_tasks_table.R** — a script to build a data frame of tasks of federal categories. Required by Compare_cats.R;

* **Misc.R** — parts of the code cut from *Compare_cats.R*, which we didn't want to delete totally. If one runs this file, he will probably see something interesting, but in general, this file is not very useful, and the analysis here wasn't included in the final version of the article;

* **Prepare_comparison_table.R** — a helper script which was used to generate the ODS to fill in with grades;

* **Prepare_list_of_regional_cats_with_levels.R** — a script which loads a list of regional categories with their levels. Required by Compare_cats.R;

* **Readme.md** — this readme;

* **Regional_categories.ods** — a table with a list of all regional categories and metadata: codes for regions, federal districts, sources of information, update dates;

* **Table_of_tasks_of_federal_categories.ods** — a short version of table of tasks of federal categories (“short” means that one cell may include multiple values), which is loaded in the *Article.Rmd* and displayed as a part of “Materials and Methods”;

* **antinomii.csl** — a citation style file for Antinomii (a modified version of GOST R 7.0.5-2008 CSL);

* **rcats_references.bib** — bibliography in BibTeX;

* **reference-styles.odt** — an ODT with reference styles to knit the article;

* **references.odt** — an ODT with “References” list (a modified version of bibliography); unfortunately, we don't know how to automatically generate both Bibliography (in Russian) and References (in English), so References is made manually and then manually inserted to the and of the article;

* **template.opendocument** — a Pandoc template file used in article knitting. It is necessary to make a use of udk_code, abstract and keywords variables, as well as to change the default place of the title and the list of authors.

### The knitting process

When a user opens *Article.Rmd* and clicks “Knit” button, the *Compare_cats.R* is sourced. *Compare_cats.R* itself sources three files: *Calculate_agreement_and_aggregate_grades.R*, *Fed_cats_tasks_table.R*, and *Prepare_list_of_regional_cats_with_levels.R*. The first of these files loads three ODS tables with by-objective grades of regional categories *(Comparison_table_raw_{suffix})*, performs necessary aggregations and writes the result to *Comparison_table_raw.csv* (this is a file autumatically generated by the script, so it's not included in this repository). The second file prepares a data frame with by-objective grades of federal categories. The third file prepares a data frame with regional categories' names and levels. After these scripts are executed, the rest of *Compare_cats.R* is processed, all comparison is made and data frames with the results of the comparison and a plot is created. Also, *Compare_cats.R* saves a *Comparison_results.csv* file with similariry indices and the most similar federal categories for each regional category. When *Compare_cats.R* is processed, the *Article.Rmd* is knit to *Article.odt*, which is an output file.

### Dependencies

* readODS — to load ODS;
* dplyr — to facilitate data processing;
* stringr — to build one of the tables;
* irr — to calculate Fleiss' kappa;
* rmarkdown — because the article is written with RMarkdown;
* knitr — for the same reason (and to display tables with *kable* function);
* ggplot2 — for the plot;
* tidyr — used in *Misc.R;*
* Hmisc — its *capitalize* function is used to create one of the tables;
* psych — its *describe* function calculates descriptive statistics for two sets of similarity indices: within federal categories and between federal and regional categories.

## [ru] Данные и код для статьи, посвящённой сравнению федеральных и региональных категорий особо охраняемых природных территорий в России

Авторы: Павел Сёмин (Habr.com), Юлия Киль и Валерия Синицына (Уральский государственный юридический университет).

Статья опубликована в журнале «Антиномии», № 4, 2020. Её можно скачать по адресу https://doi.org/10.24411/2686-7206-2020-10408.

Статья написана с помощью R и RMarkdown. Исходный код на R разделён на несколько файлов. Некоторые из этих файлов предназначены для создания шаблонов, поэтому они запускались только один раз. Другие файлы обрабатывают данные и образуют конвейер, который собирает статью. Основной файл — это **Article.Rmd**: он загружает все необходимые зависимости, поэтому достаточно просто открыть этот файл в RStudio и нажать “Knit”, чтобы собрать статью.

### Подробное описание файлов в этом репозитории

* **npa** — папка с правовой информацией о региональных категориях;

* **.Rprofile** — файл, в котором подгружаются библиотеки, необходимые для работы скриптов. В принципе они же подгружаются и в самих скриптах, но этот файл позволяет загружать их сразу при запуске RStudio;

* **Article.Rmd** — **основной файл** статьи;

* **Calculate_agreement_and_aggregate_grades.R** — скрипт, который загружает три таблицы с оценками, считает каппу Фляйса, агрегирует три оценки в одну и записывает их в csv-файл. Этот скрипт необходим для работы *Compare_cats.R;*

* **Cats_similarity.Rproj** — файл проекта;

* **Compare_Stishov_with_our_results.ods** — таблица со сравнением наших результатов с результатами из книги М. С. Стишова и Н. Дадли (Стишов М. С., Дадли Н. Охраняемые природные территории Российской Федерации и их категории. М.: WWF, 2018);

* **Compare_cats.R** — скрипт, осуществляющий сравнение и готовящий наборы данных и график для статьи. По сути это основной скрипт. “Cats” значит “**cat**egorie**s**”, с котами никак не связано :-)

* **Compare_with_Stishov.R** — вспомогательный скрипт, чтобы сделать таблицу *Compare_Stishov_with_our_results.ods*. По сути он просто ищет строки с совпадающими названиями категорий в наших результатах;

* **Comparison_results.csv** — таблица с индексами сходства и аналогичными категориями для каждой региональной категории (кроме тех, которые мы не смогли оценить из-за нехватки данных). Она автоматически создаётся при каждом запуске *Compare_cats.R*, поэтому её можно было не включать в этот репозиторий, но для удобства тех, кто не захочет устанавливать R, но все равно захочет ознакомиться с детальными результатами, мы её включили;

* **Comparison_table_raw_{suffix}.ods** — 
оценки региональных категорий по задачам, сделанные каждым из авторов: p — Павел С., j — Юлия К., l — Валерия С.;

* **Create_odt_for_reference_styles.Rmd** — документ на RMarkdown, который нужен для того, чтобы получить эталонный ODT с набором стилей и отредактировать эти стили. Эталонный ODT с отредактированными стилями сохранён как *reference-styles.odt* (см. дальше);

* **Fed_cats_tasks_table.R** — скрипт, который строит набор данных (по сути таблицу) с задачами федеральных категорий. Этот скрипт необходим для работы *Compare_cats.R;*

* **Misc.R** — куски кода из *Compare_cats.R*, которые по большому счёту не нужны, но которые было жалко удалить. Возможно, в этом файле тоже делается какой-то интересный анализ, но его результаты не так уж и важны, поэтому не были включены в итоговый вариант статьи;

* **Prepare_comparison_table.R** — вспомогательный скрипт, который делает шаблон таблицы для выставления оценок по задачам каждой категории;

* **Prepare_list_of_regional_cats_with_levels.R** — скрипт, который загружает список региональных категорий с их значением (региональное или местное). Этот скрипт необходим для работы *Compare_cats.R;*

* **Readme.md** — тот файл, который вы сейчас читаете;

* **Regional_categories.ods** — таблица с полным списком региональных категорий и метаданными: кодами субъектов, федеральных округов, источниками информации и датами её обновления;

* **Table_of_tasks_of_federal_categories.ods** — таблица задач федеральных категорий в кратком формате (в одной ячейке может быть несколько значений), которая загружается в *Article.Rmd* и отображается в разделе «Материалы и методы»;

* **antinomii.csl** — файл со стилем цитирования для «Антиномий» (модифицированный стиль для ГОСТ Р 7.0.5-2008);

* **rcats_references.bib** — библиография в BibTeX;

* **reference-styles.odt** — ODT-файл с эталонными стилями. Нужен, чтобы результат сборки *Article.Rmd* выглядел не слишком ужасно (хотя таблицам это все равно не помогло);

* **references.odt** — ODT-файл со списком References. К сожалению, у нас не получилось собрать автоматически два списка — библиографию и References — поэтому мы сделали References вручную и потом вставили в итоговый файл;

* **template.opendocument** — шаблон Pandoc, который используется для сборки статьи. Он нужен, чтобы использовать переменные udc_code, abstract и keywords, а также поменять местоположение заголовка и списка авторов по сравнению с вариантом по умолчанию.

### В какой последовательности идёт сборка статьи

Пользователь открывает *Article.Rmd* в RStudio и нажимает кнопку “Knit”. Почти сразу после этого загружается файл *Compare_cats.R*. Сам по себе *Compare_cats.R* загружает три файла: *Calculate_agreement_and_aggregate_grades.R*, *Fed_cats_tasks_table.R* и *Prepare_list_of_regional_cats_with_levels.R*. Первый из них открывает три таблицы с оценками задач каждой региональной категории *(Comparison_table_raw_{suffix})*, агрегирует их и записывает результат в *Comparison_table_raw.csv* (это автоматически создаваемый файл, поэтому нет необходимости включать его в этот репозиторий отдельно). Второй файл создаёт набор данных с оценками задач федеральных категорий. Третий файл загружает список региональных категорий с их значением. Далее выполняется оставшаяся часть *Compare_cats.R*: осуществляется сравнение, создаются наборы данных (по сути таблицы) и график для отображения в статье. Кроме того, *Compare_cats.R* создаёт файл *Comparison_results.csv* с индексами сходства и наиболее похожими федеральными категориями для каждой региональной категории. Когда обработка *Compare_cats.R* завершена, *Article.Rmd* собирается в *Article.odt*, который является итоговым файлом.

### Необходимые библиотеки

* readODS — чтобы открыть ODS;
* dplyr — упрощает обработку данных;
* stringr — нужна при создании одной из таблиц;
* irr — считает каппу Фляйса;
* rmarkdown — потому что статья написана на RMarkdown;
* knitr — по той же причине: нужна, чтобы собрать статью (в частности, показать таблицы с помощью функции *kable*);
* ggplot2 — делает график;
* tidyr — используется в *Misc.R;*
* Hmisc — содержит функцию *capitalize*, которая используется при создании одной из таблиц;
* psych — содержит функцию *describe*, которая нужна для вывода описательной статистики по наборам индексов сходства.
