#' ---
#' title: aula 03 - estrutura e manipulacao de dados na linguagem R
#' author: mauricio vancine
#' date: 2021-10-20
#' ---

# topics ------------------------------------------------------------------

# 1. Atributos dos objetos
# 2. Manipulação de dados unidimensionais
# 3. Manipulação de dados multidimensionais
# 4. Valores faltantes e especiais
# 5. Diretório de trabalho
# 6. Importar dados
# 7. Conferência de dados importados
# 8. Exportar dados

# 1. atributos dos objetos -----------------------------------------------
# atribuicao
# palavra <- dados

## atribuicao - simbolo (<-)
obj_10 <- 10 
obj_10

## atributos
library(vegan)
data(dune)
attributes(dune)

# modos dos objetos
# numeric: numeros decimais ou inteiros
# double
obj_num_dou <- 1
obj_num_dou

# mode
mode(obj_num_dou)

# type
typeof(obj_num_dou)

# numeric: numeros decimais ou inteiros
# integer
obj_num_int <- 1L
obj_num_int

# mode
mode(obj_num_int)

# type
typeof(obj_num_int)

# character: texto
# character
obj_cha <- "a" # atencao para as aspas
obj_cha

# mode
mode(obj_cha)

# logical: assume apenas dois valores (booleano)
# logical
obj_log <- TRUE # maiusculas e sem aspas
obj_log

# mode
mode(obj_log)

# complex: numeros complexos
obj_com <- 1+1i # parte imaginaria
obj_com

# mode
mode(obj_com)

## verificar o modo dos objetos
# is.numeric()
# is.integer()
# is.character()
# is.logical()
# is.complex()

## conversoes entre modos
# as.numeric()
# as.integer()
# as.character()
# as.logical()
# as.complex()

# exemplo
as.character(obj_num_dou)

# estrutura dos objetos 
# 1. vector: homogeneo (um modo) e unidimensional (uma dimensao)

# concatenar elementos
temp <- c(15, 18, 20, 22, 18)
temp

# concatenar elementos de texto
amos <- c("amostra_01", "amostra_02", "amostra_03", "amostra_04")
amos

# sequencia unitaria (x1:x2)
se <- 1:10
se

# sequencia com diferentes espacamentos 
se_e <- seq(from = 0, to = 100, by = 10) 
se_e

# rep(x, times) # repete x tantas vezes
rep_times <- rep(x = c(1, 2), times = 5)
rep_times

# rep(x, each) # retete x tantas vezes de cada
rep_each <- rep(x = c("a", "b"), each = 5)
rep_each

# palavra e sequencia numerica - sem separacao
am1 <- paste("amostra", 1:5, sep = "0")
am1

# palavra e sequencia numerica - separacao por "_"
am2 <- paste("amostra", 1:5, sep = "_0")
am2

# amostragem aleatória - sem reposição
sa_sem_rep <- sample(1:100, 10)
sa_sem_rep

# amostragem aleatória - com reposição
sa_com_rep <- sample(1:10, 100, replace = TRUE)
sa_com_rep

# exercicio 05 ------------------------------------------------------------




# -------------------------------------------------------------------------

# coercao: vetor com elementos de modos diferentes
ve <- c(1, "a", 3)
ve

ve <- c(1, "a", TRUE)
ve

# DOMINANTE character >> numeric >> logical RECESSIVO

# conversao
# funcoes
# as.character()
# as.integer()
# as.numeric()
# as.double()
# as.integer()
# as.logical()

# 2. factor: homogeneo (um modo - sempre numeric), unidimensional (uma dimensao) e possui ainda levels (niveis)
# factor nominal: variaveis nominais
fa_no <- factor(x = sample(x = c("floresta", "pastagem", "cerrado"), 
                           size = 20, replace = TRUE),
                levels = c("floresta", "pastagem", "cerrado"))
fa_no

levels(fa_no)

# factor ordinal: variaveis ordinais
fa_or <- factor(x = sample(x = c("baixa", "media", "alta"), 
                           size = 20, replace = TRUE),
                levels = c("baixa", "media", "alta"), ordered = TRUE)
fa_or

levels(fa_or)

# factor: conversao
# vector
ve_ch <- c("alta", "media", "baixa", "baixa", "media")
ve_ch

mode(ve_ch)
class(ve_ch)

# factor nominal
fa_no <- as.factor(ve_ch)
fa_no

levels(fa_no)
class(fa_no)

# exercicio 06 ------------------------------------------------------------




# -------------------------------------------------------------------------

# 3. matrix: homogeneo (um modo) e bidimensional (duas dimensao)
# 1 dispondo elementos
# matriz - funcao matrix
# vetor
ve <- 1:12

# matrix - preenchimento por linhas - horizontal
ma_row <- matrix(data = ve, nrow = 4, ncol = 3, byrow = TRUE)
ma_row

# matrix - preenchimento por colunas - vertical
ma_col <- matrix(data = ve, nrow = 4, ncol = 3, byrow = FALSE)
ma_col

#  2 combinando vetores
# `rbind`: combina vetores por linha, i.e., vetor embaixo do outro
# `cbind`: combina vetores por coluna, i.e., vetor ao lado do outro

# criar dois vetores
vec_1 <- c(1, 2, 3)
vec_2 <- c(4, 5, 6)

# combinar por linhas - vertical - um embaixo do outro
ma_rbind <- rbind(vec_1, vec_2)
ma_rbind

# combinar por colunas - horizontal - um ao lado do outro
ma_cbind <- cbind(vec_1, vec_2)
ma_cbind

# exercicio 07 ------------------------------------------------------------




# -------------------------------------------------------------------------

# 4. array: homogeneo (um modo) e multidimensional (mais que duas dimensoes)
# 1 Dispondo elementos
# `array`: dispõem um vetor em um certo numero de linhas, colunas e dimensões

# vetor
ve <- 1:8
ve

# array
ar <- array(data = ve, dim = c(2, 2, 2))
ar

# 5. data frame: heterogeneo (mais de um modo) e bidimensional (duas dimensões)
# 1 Combinando vetores horizontalmente
# `data.frame`: combina vetores horizontalmente, um ao lado do outro. Semelhante à funcao `cbind`

# criar tres vetores
vec_ch <- c("sp1", "sp2", "sp3")
vec_nu <- c(4, 5, 6)
vec_fa <- factor(c("campo", "floresta", "floresta"))

# data.frame - combinar por colunas - horizontal - um ao lado do outro
df <- data.frame(vec_ch, vec_nu, vec_fa)
df

# data.frame - nome das colunas
df <- data.frame(especies = vec_ch, 
                 abundancia = vec_nu, 
                 vegetacao = vec_fa)
df

# data frame vs cbind
# vetores
pa <- paste("parcela", 1:4, sep = "_")
pa

pe <- sample(0:1, 4, rep = TRUE)
pe

tr <- factor(rep(c("trat", "cont"), each = 2))
tr

# uniao de vetores
df <- data.frame(pa, pe, tr)
df

str(df)

# uniao de vetores
df_c <- cbind(pa, pe, tr)
df_c

str(df_c)

# exercicio 08 ------------------------------------------------------------



# -------------------------------------------------------------------------

# 6. list: heterogeneo (mais de um modo) e unidimensional (uma dimensao)
# lista
li <- list(rep(1, 20), # vector
           factor(1, 1), # factor
           cbind(c(1, 2), c(1, 2))) # matrix
li

# lista com nomes
li <- list(vector = rep(1, 20), # vector
           factor = factor(1, 1), # factor
           matrix = cbind(c(1, 2), c(1, 2))) # matrix
li


# 2. manipulacao de dados unidimensionais -------------------------------------

# vetor e fator
# indexacao []

# fixar a amostragem
set.seed(42)

# amostrar 10 elementos de uma sequencia
ve <- sample(x = seq(0, 2, .05), size = 10)
ve

# seleciona o quinto elemento
ve[5]

# seleciona os elementos de 1 a 5
ve[1:5] 

# seleciona os elementos 1 e 10 e atribui
ve_sel <- ve[c(1, 10)]
ve_sel

# retira o decimo elemento
ve[-10]

# retira os elementos 2 a 9
ve[-(2:9)]

# retira os elementos 5 e 10 e atribui
ve_sub <- ve[-c(5, 10)]
ve_sub

# selecao condicional: selecionar elementos por condicões 
# dois vetores
foo <- 42
bar <- 23

# operadores relacionais - saidas booleanas (TRUE ou FALSE)
foo == bar # igualdade
foo != bar # diferenca 
foo > bar # maior
foo >= bar # maior ou igual
foo < bar # menor
foo <= bar # menor ou igual

# quais valores sao maiores que 1?
ve > 1 

# valores acima de 1
ve[ve > 1] 

# atribui valores maiores que 1
ve_maior1 <- ve[ve > 1]
ve_maior1

# maximo
max(ve)

# minimo
min(ve)

# amplitude
range(ve)

# comprimento
length(ve)

# ordenar crescente
sort(ve)

# ordenar decrescente
sort(ve, dec = TRUE)

# arredondamento
round(ve, digits = 1)

# arredondamento
round(ve, digits = 0)

# algum?
any(ve > 1)

# todos?
all(ve > 1)

# qual(is)?
which(ve > 1)

# subconjunto
subset(ve, ve > 1)

# condicao para uma operacao
ifelse(ve > 1, 1, 0)

# indexacao []
# lista
li <- list(elem1 = 1, elem2 = 2, elem3 = 3)
li

# acessar o primeiro elemento
li[1]

# acessar o primeiro e o terceiro elementos e atribuir
li2 <- li[c(1, 3)]
li2

# retirar o primeiro elemento
li[-1]

# retirar o segundo elemento e atribuir
li_13 <- li[-2]
li_13

# valor do primeiro elemento
li[[1]]

# valor do segundo elemento e atribuir
li2_val <- li[[2]]
li2_val

# acessar o primeiro elemento
li$elem1

# acessar o primeiro e o terceiro elementos e atribuir
li1 <- li$elem1
li1

# comprimento
length(li)

# names
names(li)

# renomear
names(li) <- paste0("elemento0", 1:3)
li

# 3. manipulacao de dados bidimensionais --------------------------------------
# matriz - indexacao []
ma <- matrix(1:12, 4, 3)
ma 

# linha 3
ma[3, ] 

# coluna 2
ma[, 2] 

# elemento da linha 1 e coluna 2
ma[1, 2] 

# elementos da linha 1 e coluna 1 e 2
ma[1, 1:2] 

# elementos da linha 1 e coluna 1 e 3
ma[1, c(1, 3)] 

# elementos da linha 1 e coluna 1 e 3 e atribuir
ma_sel <- ma[1, c(1, 3)]
ma_sel

# data frame - indexacao $
# criar tres vetores
sp <- paste("sp", 1:10, sep = "")
abu <- 1:10
flo <- factor(rep(c("campo", "floresta"), each = 5))

# data frame
df <- data.frame(sp, abu, flo)
df

# $ funciona apenas para data frame 
df$sp 
df$abu
df$flo

# funcoes para uma coluna indexada por $
length(df$abu)
max(df$abu)
min(df$abu)
range(df$abu)

# converter colunas
mode(df$abu)
df$abu <- as.character(df$abu)

df$abu
mode(df$abu)

# converter colunas
df$abu <- as.numeric(df$abu)
df$abu
mode(df$abu)

# adicionar uma coluna
set.seed(42)
df$abu2 <- sample(0:1, nrow(df), rep = TRUE)
df$abu2
df

# selecionar linhas de uma matriz ou data frame 
df[df$abu > 4, ]

# selecionar linhas de uma matriz ou data frame 
df[df$abu2 == 0, ]

# selecionar linhas de uma matriz ou data frame 
df[df$flo == "floresta", ]

# funcoes de visualizacao e manipulacao
# head(): mostra as primeiras 6 linhas
# tail(): mostra as últimas 6 linhas
# nrow(): mostra o número de linhas
# ncol(): mostra o número de colunas
# dim(): mostra o número de linhas e de colunas
# rownames(): mostra os nomes das linhas (locais)
# colnames(): mostra os nomes das colunas (variáveis)
# str(): mostra as classes de cada coluna (estrutura)
# summary(): mostra um resumo dos valores de cada coluna
# rowSums(): calcula a soma das linhas (horizontal)
# colSums(): calcula a soma das colunas (vertical)
# rowMeans(): calcula a média das linhas (horizontal)
# colMeans(): calcula a média das colunas (vertical)

# 4 valores faltantes e especiais --------------------------------------
# 1 na - not available
foo_na <- NA
foo_na

# data frame
df <- data.frame(var1 = c(1, 4, 2, NA), var2 = c(1, 4, 5, 2))
df

# verificar a presenca/ausencia de NA's
is.na(df)

# verificar a presenca de algum NA's
any(is.na(df))

# retirar as linhas que possuem NA's
df_sem_na <- na.omit(df)
df_sem_na

# numero de linhas
nrow(df)
nrow(df_sem_na)

# substituir na por 0
df[is.na(df)] <- 0
df

# 2 nan - not a number
0/0
log(-1)

# criar um vetor
ve <- c(1, 2, 3, NA, NaN)
ve

# verificar a presenca de na
is.na(ve)

# verificar a presenca de nan
is.nan(ve)

# 3 inf - infinito
# limite matematico
1/0

# numero grande
10^310

# objeto nulo
nulo <- NULL
nulo

# 5. diretorio de trabalho -----------------------------------------------
# definir o diretorio de trabalho
setwd("/home/mude/data/github/course-geospatial-data-r/03_dados/tabelas")
  
# verificar o diretorio
getwd()

# listar os arquivos
dir()

# 6. importar dados ------------------------------------------------------
# ler uma planilha eletronica (.csv)
read.csv("ATLANTIC_AMPHIBIANS_sites.csv", encoding = "latin1")

# ler e atribuir uma planilha eletronica (.csv) a um objeto
da <- read.csv("ATLANTIC_AMPHIBIANS_sites.csv", encoding = "latin1")

# ver os dados
da

# conferir a classe
class(da)

# ler e atribuir uma planilha simples (.txt) a um objeto
da <- read.table("ATLANTIC_AMPHIBIANS_sites.txt", row.names = 1, header = TRUE, sep = "\t")
da

# pacote openxlsx
# install.packages("openxlsx")
library(openxlsx)

# ler e atribuir uma planilha eletronica (.xlsx) a um objeto
da <- openxlsx::read.xlsx("ATLANTIC_AMPHIBIANS_sites.xlsx", sheet = 1)
da

# 7. Conferir e manejar dados importados ---------------------------------

# conjunto de funcoes para conferir os dados
# head(): mostra as primeiras 6 linhas
# tail(): mostra as ultimas 6 linhas
# nrow(): mostra o numero de linhas
# ncol(): mostra o numero de colunas
# dim(): mostra o numero de linhas e de colunas
# rownames(): mostra os nomes das linhas (locais)
# colnames(): mostra os nomes das colunas (variaveis)
# str(): mostra as classes de cada coluna (estrutura)
# summary(): mostra um resumo dos valores de cada coluna

# head(): mostra as primeiras 10 linhas
head(da)

# head(): mostra as primeiras 10 linhas
head(da, 10)

# tail(): mostra as ultimas 6 linhas
tail(da)

# nrow(): mostra o numero de linhas
nrow(da)

# ncol(): mostra o numero de colunas
ncol(da)

# dim(): mostra o numero de linhas e de colunas
dim(da)

# rownames(): mostra os nomes das linhas (locais)
rownames(da)

# mudar nome das linhas
rownames(da) <- da$id
da

# colnames(): mostra os nomes das colunas (variaveis)
colnames(da)

# str(): mostra as classes de cada coluna (estrutura)
str(da)

# summary(): mostra um resumo dos valores de cada coluna
summary(da)

# Verificar a presenca de NAs
# algum?
any(is.na(da))

# quais?
which(is.na(da))

# retirar os NAs
da_na <- na.omit(da)

# numero de linhas
nrow(da)
nrow(da_na)

# subset das linhas com amostragens de anfibios em sao paulo
da_sp <- da[da$state == "São Paulo", ]
da_sp

# 8. Exportar dados -----------------------------------------------------

# exportar planilha eletronica (.csv)
write.csv(da_sp, "ATLANTIC_AMPHIBIAN_sites_sao_paulo.csv", 
          row.names = FALSE)


# exportar planilha de texto (.txt)
write.table(da_sp, "ATLANTIC_AMPHIBIAN_sites_sao_paulo.txt", 
            row.names = FALSE, quote = FALSE, sep = ";")

# exportar planilha eletronica (.xlsx)
openxlsx::write.xlsx(da_sp, "ATLANTIC_AMPHIBIAN_sites_sao_paulo.xlsx", 
                     row.names = FALSE, quote = FALSE)

# end ---------------------------------------------------------------------