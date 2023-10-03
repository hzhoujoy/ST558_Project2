Project2_JoyZhou
================
Joy Zhou
2023-10-03

- <a href="#01-r-markdown" id="toc-01-r-markdown">0.1 R Markdown</a>

## 0.1 R Markdown

``` r
library(rmarkdown)

rmarkdown::render("C:\\PestList\\MyDocuments\\Statistic\\ST558\\repos\\ST558_Project2\\Project2_JoyZhoy.Rmd", 
                  output_format = "github_document", 
                  output_file = "READme.md",
                  output_options = list(
                    name_value_pairs = "value",
                    toc = TRUE,
                    toc_depth = 3,
                    number_sections = TRUE,
                    df_print = "tibble"
                  )
)
```
