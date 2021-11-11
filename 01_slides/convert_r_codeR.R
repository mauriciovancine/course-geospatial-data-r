#' ---
#' title: convert to xaringan presentation to pdf
#' author: mauricio vancine
#' date: 2020-10-16
#' ---

# packages
library(knitr)

# directory
setwd("01_aulas")
dir(pattern = ".Rmd")

# convert rmarkdown
purrr::map(dir(pattern = ".Rmd"), knitr::purl)

# end ---------------------------------------------------------------------