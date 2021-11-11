#' ---
#' title: aula 04 introducao ao tidyverse
#' author: mauricio vancine
#' date: 2021-10-11
#' ---

# packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)
library(lubridate)
library(parallel)

# topics ------------------------------------------------------------------

# 1. contextualizacao
# 2. tidyverse
# 3. here
# 4. readr, readxl e writexl
# 5. tibble
# 6. magrittr (pipe - %>%)
# 7. tidyr
# 8. dplyr
# 9. stringr
# 10. forcats
# 11. lubridate
# 12. purrr

# 2. tidyverse -----------------------------------------------------------
# instalar pacote
# install.packages("tidyverse")

# carregar pacote
library(tidyverse)

# list all packages in the tidyverse 
tidyverse::tidyverse_packages(include_self = TRUE)

# 3. here -----------------------------------------------------------------
# instalar
# install.packages("here")

# carregar
library(here)

# conferir
here::here()

# criar um arquivo .here
# here::set_here("")

# 4. readr, readxl e writexl ----------------------------------------------
# formato .csv
# importar sites com here
si <- read.csv(here::here("03_dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.csv"))
si

object.size(si)

# importar sites sem here
si <- readr::read_csv("./03_dados/tabelas/ATLANTIC_AMPHIBIANS_sites.csv")
si

# formato .txt
# importar sites
si <- readr::read_tsv(here::here("03_dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.txt"))
si

# importar .xlsx
# install.packages("readxl")
library("readxl")

# exportar .xlsx
# install.packages("writexl")
library("writexl")

# importar sites
si <- readxl::read_xlsx(here::here("03_dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.xlsx"), 
                        sheet = 1)
si

# importar sites
si <- readr::read_csv(here::here("03_dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.csv"))
si

# importar especies
sp <- readr::read_csv(here::here("03_dados", "tabelas", "ATLANTIC_AMPHIBIANS_species.csv"))
sp

# 5. tibble --------------------------------------------------------------

# view the sites data
tibble::glimpse(si)

# view the species data
tibble::glimpse(sp)

# tibble vs data.frame

# 1. nunca converte um tipo character como factor - 
df <- data.frame(ch = c("a", "b"), nu = 1:2)
str(df)

tb <- tibble::tibble(ch = c("a", "b"), nu = 1:2)
glimpse(tb)

# 2. a indexacao com colchetes sempre retorna um tibble
df_ch <- df[, 1]
class(df_ch)

tb_ch <- tb[, 1]
class(tb_ch)

# indexacao pelo nome devolve um vetor
tb_ch <- tb$ch
class(tb_ch)

# 3. nao faz correspondencia parcial, retorna NULL se a coluna nao existe com o nome especificado
df$c 
tb$c

# 6. magrittr (pipe - %>%) -----------------------------------------------
# sem pipe
sqrt(sum(1:100))

# com pipe
1:100 %>% 
  sum() %>% 
  sqrt()

# fixar amostragem
set.seed(42)

# sem pipe
ve <- sum(sqrt(sort(log10(rpois(100, 10)))))
ve

# fixar amostragem
set.seed(42)

# com pipe
ve <- rpois(100, 10) %>% 
  log10() %>%
  sort() %>% 
  sqrt() %>% 
  sum()
ve  

# exercicio 09 ------------------------------------------------------------


# -------------------------------------------------------------------------

# palmerpenquins ----------------------------------------------------------

# instalar 
# install.packages("palmerpenguins")

# carregar
library(palmerpenguins)

# ajuda dos dados
?penguins
?penguins_raw

# visualizar os dados
penguins
penguins_raw

# glimpse
tibble::glimpse(penguins)
tibble::glimpse(penguins_raw)

# 7. tidyr ---------------------------------------------------------------
# funcoes
# 1. unite(): junta dados de múltiplas colunas em uma
# 2. separate(): separa caracteres em múlplica colunas
# 3. separate_rows(): separa caracteres em múlplica colunas e linhas
# 4. drop_na(): retira linhas com NA
# 5. replace_na(): substitui NA
# 6. pivot_wider(): long para wide
# 7. pivot_longer(): wide para long

# 1. unite()
# unir colunas
penguins_raw_unir <- tidyr::unite(data = penguins_raw, 
                                  col = "region_island",
                                  Region:Island, 
                                  sep = ", ",
                                  remove = FALSE)
head(penguins_raw_unir[, c("Region", "Island", "region_island")])
  
# 2. separate()
# separar colunas
penguins_raw_separar <- tidyr::separate(data = penguins_raw, 
                                        col = Stage,
                                        into = c("stage", "egg_stage"), 
                                        sep = ", ",
                                        remove = FALSE)
head(penguins_raw_separar[, c("Stage", "stage", "egg_stage")])

# 3. separate_rows()
# separar colunas em linhas
penguins_raw_separar_linhas <- tidyr::separate_rows(data = penguins_raw,
                                                    Stage,
                                                    sep = ", ")
head(penguins_raw_separar_linhas[, c("studyName", "Sample Number", "Species", 
                                     "Region", "Island", "Stage")])

# 4. drop_na()
# remover linhas com na
penguins_raw_todas_na <- tidyr::drop_na(data = penguins_raw)
head(penguins_raw_todas_na)

# remover linhas com na de uma coluna
penguins_raw_colunas_na <- tidyr::drop_na(data = penguins_raw,
                                          any_of("Comments"))
head(penguins_raw_colunas_na[, "Comments"])

# 5. replace_na()
# substituir nas por 0 em uma coluna
penguins_raw_subs_na <- tidyr::replace_na(data = penguins_raw,
                                          list(Comments = "Unknown"))
head(penguins_raw_subs_na[, "Comments"])

# 6. pivot_wider()
# long para wide
penguins_raw_pivot_wider <- tidyr::pivot_wider(data = penguins_raw[, c(2, 3, 13)], 
                                               names_from = Species, 
                                               values_from = `Body Mass (g)`)
head(penguins_raw_pivot_wider)

# long para wide
penguins_raw_pivot_wider <- tidyr::pivot_wider(data = penguins_raw[, c(2, 3, 13)], 
                                               names_from = Species, 
                                               values_from = `Body Mass (g)`,
                                               values_fill = 0)
head(penguins_raw_pivot_wider)

# 7. pivot_longer()
# wide para long
penguins_raw_pivot_longer <- tidyr::pivot_longer(data = penguins_raw[, c(2, 3, 10:13)], 
                                                 cols = `Culmen Length (mm)`:`Body Mass (g)`,
                                                 names_to = "medidas", 
                                                 values_to = "valores")
head(penguins_raw_pivot_longer)

# exercicio 10 ------------------------------------------------------------


# exercicio 11 ------------------------------------------------------------


# 8. dplyr ---------------------------------------------------------------
# funcoes
# 1. relocate(): muda a ordem das colunas
# 2. rename(): muda o nome das colunas
# 3. select(): seleciona colunas pelo nome ou posição
# 4. pull(): seleciona uma coluna como vetor
# 5. mutate(): adiciona novas colunas ou resultados em colunas existentes
# 6. arrange(): reordena as linhas com base nos valores de colunas
# 7. filter(): seleciona linhas com base em valores de colunas
# 8. slice(): seleciona linhas de diferente formas
# 9. distinct(): remove linhas com valores repetidos com base nos valores de colunas
# 10. count(): conta observações para um grupo
# 11. group_by(): agrupa linhas pelos valores das colunas
# 12. summarise(): resume os dados através de funções considerando valores das colunas
# 13. *_join(): funções que juntam dados de duas tabelas através de uma coluna chave

# 1. relocate()
# reordenar colunas - posicao
penguins_relocate_ncol <- penguins %>% 
  dplyr::relocate(sex, year, .after = 2)
head(penguins_relocate_ncol)

# 2. rename()
# renomear colunas
penguins_rename <- penguins %>% 
  dplyr::rename(bill_length = bill_length_mm,
                bill_depth = bill_depth_mm,
                flipper_length = flipper_length_mm,
                body_mass = body_mass_g)
head(penguins_rename)

# renomear todas as colunas
penguins_rename_with <- penguins %>% 
  dplyr::rename_with(toupper)
head(penguins_rename_with)

# 3. select()
# selecionar colunas por posicao
penguins_select_position <- penguins %>% 
  dplyr::select(3:6)
head(penguins_select_position)

# selecionar colunas por nomes
penguins_select_names <- penguins %>% 
  dplyr::select(bill_length_mm:body_mass_g)
head(penguins_select_names)

# remover colunas pelo nome
penguins_select_names_remove <- penguins %>% 
  dplyr::select(-(bill_length_mm:body_mass_g))
head(penguins_select_names_remove)

# selecionar colunas por padrao - starts_with(), ends_with() e contains()
penguins_select_contains <- penguins %>% 
  dplyr::select(contains("_mm"))
head(penguins_select_contains)

# 4. pull()
# coluna para vetor
penguins_select_pull <- penguins %>% 
  dplyr::pull(bill_length_mm)
head(penguins_select_pull, 15)

# 5. mutate()
# adicionar colunas
penguins_mutate <- penguins %>% 
  dplyr::mutate(body_mass_kg = body_mass_g/1e3, .before = sex)
head(penguins_mutate)

## modificar varias colunas
penguins_mutate_across <- penguins %>% 
  
  dplyr::mutate(across(where(is.factor), as.character))
head(penguins_mutate_across)

# 6. arrange()
# reordenar os valores por ordem crescente
penguins_arrange <- penguins %>% 
  dplyr::arrange(body_mass_g)
head(penguins_arrange)

# reordenar os valores por ordem decrescente
penguins_arrange_desc <- penguins %>% 
  dplyr::arrange(desc(body_mass_g))
head(penguins_arrange_desc)

# reordenar os valores por ordem decrescente
penguins_arrange_desc_m <- penguins %>% 
  dplyr::arrange(-body_mass_g)
head(penguins_arrange_desc_m)

# reordenar os valores por ordem crescente de varias colunas
penguins_arrange_across <- penguins %>% 
  dplyr::arrange(across(where(is.numeric)))
head(penguins_arrange_across)

# 7. filter()
# filtrar linhas por valores de uma coluna
penguins_filter <- penguins %>% 
  dplyr::filter(body_mass_g >= 5000)
head(penguins_filter)

# filtrar linhas por valores de duas colunas
penguins_filter_two <- penguins %>% 
  dplyr::filter(species == "Adelie" & sex == "female")
head(penguins_filter_two)

# filtrar linhas por mais de um valor e mais de uma coluna
penguins_filter_in <- penguins %>% 
  dplyr::filter(species %in% c("Adelie", "Gentoo"),
                sex == "female")
head(penguins_filter_in)

# filtrar linhas por nas
penguins_filter_na <- penguins %>% 
  dplyr::filter(!is.na(sex) == TRUE)
head(penguins_filter_na)

# filtrar linhas por valores em um intervalo
penguins_filter_between <- penguins %>% 
  dplyr::filter(between(body_mass_g, 3000, 4000))
head(penguins_filter_between)

# filtrar linhas por valores de varias colunas
penguins_filter_if <- penguins %>% 
  dplyr::filter(if_all(where(is.integer), ~ . > 200))
head(penguins_filter_if)

# 8. slice()
# selecionar linhas por intervalos
penguins_slice <- penguins %>% 
  dplyr::slice(n = c(1, 3, 300:n()))
head(penguins_slice)

# selecionar linhas iniciais
penguins_slice_head <- penguins %>% 
  dplyr::slice_head(n = 5)
head(penguins_slice_head)

# selecionar linhas por valores de uma coluna
penguins_slice_max <- penguins %>% 
  dplyr::slice_max(body_mass_g, n = 5)
head(penguins_slice_max)

# selecionar linhas aleatoriamente
penguins_slice_sample <- penguins %>% 
  dplyr::slice_sample(n = 30)
head(penguins_slice_sample)

# 9. distinct()
# retirar linhas com valores duplicados
penguins_distinct <- penguins %>% 
  dplyr::distinct(body_mass_g)
head(penguins_distinct)

# retirar linhas com valores duplicados e manter colunas
penguins_distinct_keep_all <- penguins %>% 
  dplyr::distinct(body_mass_g, .keep_all = TRUE)
head(penguins_distinct_keep_all)

# retirar linhas com valores duplicados para varias colunas
penguins_distinct_keep_all_across <- penguins %>% 
  dplyr::distinct(across(where(is.integer)), .keep_all = TRUE)
head(penguins_distinct_keep_all_across)

# 10. count()
# contagens de valores para uma coluna
penguins_count <- penguins %>% 
  dplyr::count(species)
penguins_count

# contagens de valores para mais de uma coluna
penguins_count_two <- penguins %>% 
  dplyr::count(species, island)
penguins_count_two

# 11. group_by()
# agrupamento
penguins_group_by <- penguins %>% 
  dplyr::group_by(species)
head(penguins_group_by)

# agrupamento de várias colunas
penguins_group_by_across <- penguins %>% 
  dplyr::group_by(across(where(is.factor)))
head(penguins_group_by_across)

# 12. summarise()
# resumo
penguins_summarise <- penguins %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarize(body_mass_g_mean = mean(body_mass_g, na.rm = TRUE),
                   body_mass_g_sd = sd(body_mass_g, na.rm = TRUE))
penguins_summarise

# resumo para várias colunas
penguins_summarise_across <- penguins %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
penguins_summarise_across

penguins_summarise <- penguins %>% 
  dplyr::group_by(species, island) %>% 
  dplyr::summarize(body_mass_g_mean = mean(body_mass_g, na.rm = TRUE),
                   body_mass_g_sd = sd(body_mass_g, na.rm = TRUE))
penguins_summarise

# 13. bind_rows() e bind_cols()
# selecionar as linhas para dois tibbles
penguins_01 <- dplyr::slice(penguins, 1:5) %>% dplyr::select(1:3)
penguins_02 <- dplyr::slice(penguins, 51:55) %>% dplyr::select(4:6)

# combinar as linhas
penguins_bind_rows <- dplyr::bind_rows(penguins_01, penguins_02, .id = "id")
head(penguins_bind_rows)

## combinar as colunas
penguins_bind_cols <- dplyr::bind_cols(penguins_01, penguins_02, .name_repair = "unique")
head(penguins_bind_cols)

# 14. *_join()
## coordenadas
penguin_islands <- tibble(
  island = c("Torgersen", "Biscoe", "Dream", "Alpha"),
  longitude = c(-64.083333, -63.775636, -64.233333, -63),
  latitude = c(-64.766667, -64.818569, -64.733333, -64.316667))
penguin_islands

# juncao - left
penguins_left_join <- dplyr::left_join(penguins, penguin_islands, by = "island")
head(penguins_left_join)

# manipular dados
# selecionar colunas
penguins_dplyr <- penguins %>% 
  dplyr::select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
penguins_dplyr

# selecionar colunas e retirar linhas com nas
penguins_dplyr <- penguins %>% 
  dplyr::select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>% 
  dplyr::filter(if_all(where(is.numeric), ~ !is.na(.)))
penguins_dplyr

# selecionar colunas, retirar linhas com nas e calcular a media das colunas para as especies
penguins_dplyr <- penguins %>% 
  dplyr::select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>% 
  dplyr::filter(if_all(where(is.numeric), ~ !is.na(.))) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
penguins_dplyr

# exercicio 12 ------------------------------------------------------------

# exercicio 13 ------------------------------------------------------------

# exercicio 14 ------------------------------------------------------------

# exercicio 15 ------------------------------------------------------------



# 9. stringr -------------------------------------------------------------

# comprimento
stringr::str_length(string = "penguins")

# substituir
stringr::str_replace(string = "penguins", pattern = "i", replacement = "y")

# separar
stringr::str_split(string = "p-e-n-g-u-i-n-s", pattern = "-", simplify = TRUE)

  # extrair pela posicao
stringr::str_sub(string = "penguins", end = 3)

# extrair por padrao
stringr::str_extract(string = "penguins", pattern = "p")

# inserir espaco em branco - esquerda
stringr::str_pad(string = "penguins", width = 10, side = "left")

# inserir espaco em branco - direita
stringr::str_pad(string = "penguins", width = 10, side = "right")

# inserir espaco em branco - ambos
stringr::str_pad(string = "penguins", width = 10, side = "both")

# remover espacos em branco - esquerda
stringr::str_trim(string = " penguins ", side = "left")

# remover espacos em branco - direta
stringr::str_trim(string = " penguins ", side = "right")

# remover espacos em branco - ambos
stringr::str_trim(string = " penguins ", side = "both")

# minusculas
stringr::str_to_lower(string = "Penguins")

# maiusculas
stringr::str_to_upper(string = "penguins")

# primeiro caracter maiusculo da primeira palavra
stringr::str_to_sentence(string = "palmer penGuins")

# primeiro caracter maiusculo de cada palavra
stringr::str_to_title(string = "palmer penGuins")

# ordenar - crescente
stringr::str_sort(x = letters)

# ordenar - decrescente
stringr::str_sort(x = letters, dec = TRUE)

# alterar valores das colunas
penguins_stringr_valores <- penguins %>% 
  dplyr::mutate(species = stringr::str_to_lower(species))

# alterar nome das colunas
penguins_stringr_nomes <- penguins %>% 
  dplyr::rename_with(stringr::str_to_title)

# 10. forcats -------------------------------------------------------------

# converter dados de string para fator
forcats::as_factor(penguins_raw$Species) %>% head()

# mudar o nome dos niveis
forcats::fct_recode(penguins$species, 
                    ade = "Adelie", 
                    chi = "Chinstrap", 
                    gen = "Gentoo") %>% head()

# inverter os niveis
forcats::fct_rev(penguins$species) %>% head()

# especificar a ordem dos niveis
forcats::fct_relevel(penguins$species, "Chinstrap", "Gentoo", "Adelie") %>% head()

# niveis pela ordem em que aparecem
forcats::fct_inorder(penguins$species) %>% head()

## ordem (decrescente) de frequecia
forcats::fct_infreq(penguins$species) %>% head()

# agregacao de niveis raros em um nivel
forcats::fct_lump(penguins$species) %>% head()

# transformar varias colunas em fator
penguins_raw_multi_factor <- penguins_raw %>% 
  dplyr::mutate(across(where(is.character), forcats::as_factor))
penguins_raw_multi_factor

# 11. lubridate ----------------------------------------------------------
# carregar
library(lubridate)

# extrair a data nesse instante
lubridate::today()

# extrair a data e tempo nesse instante
lubridate::now()

# strings e numeros para datas
lubridate::dmy("03-03-2021")

# strings e numeros para datas
lubridate::dmy("03-Mar-2021")
lubridate::dmy(03032021)
lubridate::dmy("03032021")
lubridate::dmy("03/03/2021")
lubridate::dmy("03.03.2021")

# especificar horarios e fuso horario
lubridate::dmy_h("03-03-2021 13")
lubridate::dmy_hm("03-03-2021 13:32")
lubridate::dmy_hms("03-03-2021 13:32:01")
lubridate::dmy_hms("03-03-2021 13:32:01", tz = "America/Sao_Paulo")

# dados com componentes individuais
dados <- tibble::tibble(
  ano = c(2021, 2021, 2021),
  mes = c(1, 2, 3),
  dia = c(12, 20, 31),
  hora = c(2, 14, 18), 
  minuto = c(2, 44, 55))
dados

# dados com componentes individuais
dados %>% 
  dplyr::mutate(data = lubridate::make_datetime(ano, mes, dia, hora, minuto))

# data para data-horario
lubridate::as_datetime(today())

# data-horario para data
lubridate::as_date(now())

# extrair
lubridate::year(now())
lubridate::month(now())
lubridate::month(now(), label = TRUE)
lubridate::day(now())
lubridate::wday(now())
lubridate::wday(now(), label = TRUE)
lubridate::second(now())

# incluir
# data
data <- dmy_hms("04-03-2021 01:04:56")

# incluir
lubridate::year(data) <- 2020
lubridate::month(data) <- 01
lubridate::hour(data) <- 13

# incluir varios valores
update(data, year = 2020, month = 1, mday = 1, hour = 1)

# duracoes
# subtracao de datas
tempo_estudando_r <- lubridate::today() - lubridate::dmy("30-11-2011")

# conversao para duracao
tempo_estudando_r_dur <- lubridate::as.duration(tempo_estudando_r)
tempo_estudando_r_dur

# criando duracoes
lubridate::duration(90, "seconds")
lubridate::duration(1.5, "minutes")
lubridate::duration(1, "days")

# transformacao da duracao
lubridate::dseconds(100)
lubridate::dminutes(100)
lubridate::dhours(100)
lubridate::ddays(100)
lubridate::dweeks(100)
lubridate::dyears(100)

# somando duracoes a datas
lubridate::today() + lubridate::ddays(1)

# subtraindo duracoes de datas
lubridate::today() - lubridate::dyears(1)

# multiplicando duracoes
2 * dyears(2)

# periodos
# criando periodos
period(c(90, 5), c("second", "minute"))
period(c(3, 1, 2, 13, 1), c("second", "minute", "hour", "day", "week"))

# transformacao de periodos
lubridate::seconds(100)
lubridate::minutes(100)
lubridate::hours(100)
lubridate::days(100)
lubridate::weeks(100)
lubridate::years(100)

# somando datas
lubridate::today() + lubridate::weeks(10)

# subtraindo datas
lubridate::today() - lubridate::weeks(10)

# criando datas recorrentes
lubridate::today() + lubridate::weeks(0:10)

# intervalos
# criando duas datas - inicio de estudos do R e nascimento do meu filho
r_inicio <- lubridate::dmy("30-11-2011")
filho_nascimento <- lubridate::dmy("26-09-2013")
r_hoje <- lubridate::today()

# criando intervalos - interval
r_intervalo <- lubridate::interval(r_inicio, r_hoje)

# criando intervalos - interval %--%
filho_intervalo <- filho_nascimento %--% lubridate::today()

# operacooes com intervalos
lubridate::int_start(r_intervalo)
lubridate::int_end(r_intervalo)
lubridate::int_length(r_intervalo)
lubridate::int_flip(r_intervalo)
lubridate::int_shift(r_intervalo, duration(days = 30))

# verificar sobreposicao - int_overlaps
lubridate::int_overlaps(r_intervalo, filho_intervalo)

# verificar se intervalo esta contido
r_intervalo %within% filho_intervalo

# periodos dentro de um intervalo - anos
r_intervalo / lubridate::years()
r_intervalo %/% lubridate::years()

# periodos dentro de um intervalo - dias e semandas
filho_intervalo / lubridate::days()
filho_intervalo / lubridate::weeks()

# tempo total estudando R
lubridate::as.period(r_intervalo)

# idade do meu filho
lubridate::as.period(filho_intervalo)

# fusos horarios
# fuso horario no r
Sys.timezone()

# verificar os fusos horarios
length(OlsonNames())
head(OlsonNames())

# que horas sao em...
lubridate::with_tz(lubridate::now(), tzone = "America/Sao_Paulo")
lubridate::with_tz(lubridate::now(), tzone = "GMT")
lubridate::with_tz(lubridate::now(), tzone = "Europe/Berlin")

# alterar o fuso sem mudar a hora
lubridate::force_tz(lubridate::now(), tzone = "GMT")

# 12. purrr --------------------------------------------------------------
# loop for
for(i in 1:10){
  print(i)
}

# loop for com map
purrr::map(.x = 1:10, .f = print)

# lista
x <- list(1:5, c(4, 5, 7), c(1, 1, 1), c(2, 2, 2, 2, 2))
x

# funcao map
purrr::map(x, sum)

# variacoes da funcao map
purrr::map_dbl(x, sum)
purrr::map_chr(x, paste, collapse = " ")

# listas
x <- list(3, 5, 0, 1)
y <- list(3, 5, 0, 1)
z <- list(3, 5, 0, 1)

# map2* duas litas
purrr::map2_dbl(x, y, prod)

# pmap muitas listas
purrr::pmap_dbl(list(x, y, z), prod)

# resumo dos dados
penguins %>% 
  dplyr::select(where(is.numeric)) %>% 
  tidyr::drop_na() %>% 
  purrr::map_dbl(mean)

# analise dos dados
penguins %>%
  dplyr::group_split(island, species) %>% 
  purrr::map(~ lm(bill_depth_mm ~ bill_length_mm, data = .x)) %>% 
  purrr::map(summary) %>% 
  purrr::map("r.squared")

# carregar
library(parallel)

# numero de cores
parallel::detectCores()

# end ---------------------------------------------------------------------