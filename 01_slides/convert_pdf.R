#' ---
#' title: convert to xaringan presentation to pdf
#' author: mauricio vancine
#' date: 2020-10-24
#' ---

# packages
library(pagedown)
library(xaringan)
library(tidyverse)
library(here)
library(R.utils)

# convert rmarkdown
for(i in 1:10){
  print(i)
  pagedown::chrome_print(here("01_slides", dir(path = here("01_slides"), pattern = ".Rmd"))[i], timeout = 1e6)
}

# compress pdf
for(i in 1:10){
  print(i)
  tryCatch(
    R.utils::compressPDF(filename = here("01_slides", dir(path = here("01_slides"), pattern = ".pdf$"))[i],
                         outFilename = here("01_slides", dir(path = here("01_slides"), pattern = ".pdf$"))[i],
                         overwrite = TRUE),
    error=function(e) e
  )
}

# end ---------------------------------------------------------------------