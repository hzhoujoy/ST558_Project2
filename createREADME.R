# author: Joy Zhou
# date: 10/10/2023
# purpose: Render Project2_JoyZhou.Rmd as a .md file called README.md for my repo.

library(rmarkdown)

rmarkdown::render( 
                  input = "Project2_JoyZhou.Rmd",
                  output_format = "github_document", 
                  output_file = "README.md",
                  runtime = "static",
                  clean = TRUE,
                  params = NULL,
                  knit_meta = NULL,
                  envir = parent.frame(),
                  run_pandoc = TRUE,
                  quiet = FALSE,
                  encoding = "UTF-8"
)
