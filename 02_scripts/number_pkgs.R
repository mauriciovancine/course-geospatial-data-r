
library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)

cran_link <- function(...) {
  file.path("https://cran.rstudio.com/src/contrib", ...)
}

pkgs <- read_html(cran_link()) %>% 
  html_text() %>% 
  tibble(text = unlist(str_split(., pattern = "\n"))) %>% 
  select(text) %>% 
  slice(-(1:2)) %>% 
  as.matrix() %>% 
  str_trim() %>% 
  as_tibble() %>% 
  filter(str_detect(string = value, pattern = ".tar.gz")) %>% 
  mutate(name = str_split(string = value, pattern = ".tar.gz", simplify = TRUE)[, 1],
         date = str_trim(str_split(string = value, pattern = ".tar.gz", simplify = TRUE)[, 2])) %>% 
  mutate(date = as_date(str_split(string = date, pattern = " ", simplify = TRUE)[, 1])) %>% 
  select(name, date)
pkgs
head(pkgs)

pkgs_data <- pkgs %>%
  group_by(date = floor_date(date, unit = "month")) %>%
  summarise(NewPackages = n()) %>%
  ungroup() %>%
  mutate(TotalPackages = cumsum(NewPackages))
pkgs_data

ggplot(data = pkgs_data, aes(date, TotalPackages)) +
  geom_line(size = 2, color = "midnightblue") +
  geom_hline(yintercept = pkgs_data[nrow(pkgs_data), ]$TotalPackages, lty = 2) +
  annotate("text", x = as_date("2007-01-01"), y = pkgs_data[nrow(pkgs_data), ]$TotalPackages + 1000, 
           label = pkgs_data[nrow(pkgs_data), ]$TotalPackages, size = 10) +
  labs(x = NULL, y = "Número de pacotes disponíveis",
       title = "Quantos pacotes estão disponíveis no CRAN?",
       subtitle = "Todos os pacotes já criados") +
  theme_minimal() +
  theme(title = element_text(size = 25),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
