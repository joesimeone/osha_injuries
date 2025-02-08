
library(purrr)

packages_used <- c("rvest", "janitor", "tidyverse", 
                   "data.table", "here", "readxl", "quarto")


map(packages_used, citation)
