#' ---
#' title: aula 08 - estrutura e manipulação de dados matriciais
#' author: mauricio vancine
#' date: 2021-10-25
#' ---

# packages ----------------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)
library(geobr)
library(rnaturalearth)
library(viridis)
library(RStoolbox)

# topics ------------------------------------------------------------------

# 1. principais pacotes
# 2. classes raster
# 3. importar dados matriciais
# 4. descricao de objetos raster
# 5. converter crs de objetos raster
# 6. operacoes de objetos raster
# 7. interacoes raster-vetor
# 8. conversoes raster-vetor
# 9. indices espectrais
# 10. exportar dados matriciais

# 1. principais pacotes ------------------------------------------------------
# raster
# install.packages("raster")
library(raster)

# terdem_rc
# install.packages("terdem_rc")
# library(terdem_rc)

# stars
# install.packages("stars")
# library(stars)

# 2. classes raster ------------------------------------------------------
# volcano
volcano

# rasterlayer
ra_layer <- raster::raster(volcano)
ra_layer

# plot
plot(ra_layer)

# plot
plot(ra_layer, col = viridis::viridis(10))
plot(ra_layer, col = viridis::viridis(100))

# stack
ra_layer1 <- ra_layer
ra_layer2 <- ra_layer * ra_layer
ra_layer3 <- sqrt(ra_layer)
ra_layer4 <- log10(ra_layer)
ra_stack <- raster::brick(ra_layer1, 
                          ra_layer2, 
                          ra_layer3, 
                          ra_layer4)
ra_stack

# plot
plot(ra_stack, col = viridis::viridis(100))

# brick
ra_layer1 <- ra_layer
ra_layer2 <- ra_layer * ra_layer
ra_layer3 <- sqrt(ra_layer)
ra_layer4 <- log10(ra_layer)

ra_brick <- raster::brick(ra_layer1, 
                          ra_layer2, 
                          ra_layer3, 
                          ra_layer4)
ra_brick

# plot
plot(ra_brick, col = viridis::viridis(10))

# 3. importar dados matriciais -------------------------------------------

# importar raster
dem <- raster::raster(here::here("03_dados", "raster", "srtm_27_17.tif"))
dem

# rio claro
rc_2020 <- geobr::read_municipality(code_muni = 3543907, year = 2020, showProgress = FALSE) %>% 
  sf::st_transform(crs = 4326)
rc_2020

# plot
plot(dem, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# ajuste do limite
dem_rc <- raster::crop(dem, rc_2020)
dem_rc

# plot
plot(dem_rc, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# listar arquivos
files <- dir(path = here::here("03_dados", "raster"), pattern = "wc", full.names = TRUE) %>% 
  grep(".tif", ., value = TRUE)
files

# importar
bioclim <- raster::stack(files)
bioclim

plotRGB(bioclim, b = 12, g = 15, r = 18, stretch = "hist")

# plot
plot(bioclim[[1:2]], col = viridis::viridis(10))

# importar
landsat_rc <- raster::brick(here::here("03_dados", "imagem", "landsat_rc.tif"))
landsat_rc

# plot
plot(landsat_rc$landsat_rc.2)

# plot - cores naturais
plotRGB(landsat_rc, r = 4, g = 3, b = 2, stretch = "lin")

# plot - cores naturais
plotRGB(landsat_rc, r = 4, g = 3, b = 2, stretch = "hist")

# plot - falsa cor infravermelho
plotRGB(landsat_rc, r = 5, g = 4, b = 3, stretch = "hist")

# plot - cores naturais simuladas
plotRGB(landsat_rc, r = 7, g = 6, b = 4, stretch = "hist")

# plot - cores naturais simuladas
plotRGB(landsat_rc, r = 6, g = 5, b = 4, stretch = "hist")

# plot - cores naturais simuladas
plotRGB(landsat_rc, r = 7, g = 5, b = 3, stretch = "hist")

# 4. descricao de objetos raster -----------------------------------------

# rio claro
dem_rc

# classe
class(dem_rc)

# dimensoes
dim(dem_rc)

# numero de camadas
nlayers(dem_rc)

# numero de linhas
nrow(dem_rc)

# numero de colunas
ncol(dem_rc)

# numero de celulas
ncell(dem_rc)

# resolucao do raster
res(dem_rc)

# resolucao do stack
res(bioclim)

# extensao do raster
extent(dem_rc)

# extensao do stack
extent(bioclim)

# projecao ou crs do raster
projection(dem_rc)

# projecao ou crs do stack
projection(bioclim)

# nomes do raster
names(dem_rc)

# nomes do stack
names(dem_rc)

# valores do raster
getValues(dem_rc) %>% head()
values(dem_rc) %>% head()
dem_rc[] %>% head()

# valores do raster - histograma
dev.off()
dem_rc %>% 
  raster::values() %>% 
  hist(col = "steelblue", border = "white", main = NA, 
       xlab = "Elevação (m)", ylab = "Frequência")

# valores do stack
values(bioclim[[1:3]]) %>% head()

# valores do stack - grafico pareado
bioclim[[1:3]] %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  dplyr::sample_n(1e3) %>% 
  pairs(cex = 1.4, pch = 20, col = adjustcolor("steelblue", .7))

# 5. converter crs de objetos raster -------------------------------------

# projecao do raster
raster::projection(dem_rc)
raster::crs(dem_rc)

# proj4string utm 23 s
utm23s <- "+init=epsg:31983"
utm23s

# reprojection raster
dem_rc_utm23s <- raster::projectRaster(dem_rc, crs = utm23s)
dem_rc_utm23s

# reprojection raster
dem_rc_utm23s <- raster::projectRaster(dem_rc, crs = utm23s, res = 90, method = "bilinear")
dem_rc_utm23s

# reprojection vector
rc_2020_utm23s <- sf::st_transform(rc_2020, crs = utm23s)
rc_2020_utm23s

# plot
plot(dem_rc_utm23s, col = viridis::viridis(10))
plot(rc_2020_utm23s$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# crs global
# WGS84/GCS
bioclim$wc2.1_10m_bio_1

# plot
plot(bioclim$wc2.1_10m_bio_1, col = viridis::viridis(10))

# proj4string mollweide
moll <- "+proj=moll"
moll

# reprojection
bio01_moll <- raster::projectRaster(bioclim$wc2.1_10m_bio_1, crs = moll, res = 25e3, method = "bilinear")
bio01_moll

# plot
plot(bio01_moll, col = viridis::viridis(10))

# proj4string lambert 
laea <- "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0"
laea

# reprojection
bio01_laea <- raster::projectRaster(bioclim$wc2.1_10m_bio_1, crs = laea, res = 25e3, method = "bilinear")
bio01_laea

# plot
plot(bio01_laea, col = viridis::viridis(10))

# 6. operacoes de obejtos raster -----------------------------------------

# raster - linha 1 e columna 1
dem_rc[1, 1]

# celula 1
dem_rc[1]

# stack - linha 1 e columna 1
bioclim[1, 1]

# celula 1
bioclim[1]

# selecao de camada num objeto stack utilizando a funcao subset
bioclim_bio01 <- raster::subset(bioclim, "wc2.1_10m_bio_1")
bioclim_bio01

# selecao de camada num objeto stack utilizando a funcao raster
bioclim_bio01 <- raster::raster(bioclim, layer = 1)
bioclim_bio01

# selecao de camada num objeto stack utilizando os operadores [[]] e o nome
bioclim_bio01 <- bioclim[["wc2.1_10m_bio_1"]]
bioclim_bio01

# selecao de camada num objeto stack utilizando os operadores [[]] e a posicao
bioclim_bio01 <- bioclim[[1]]
bioclim_bio01

# selecao de camada num objeto stack utilizando o operador $
bioclim_bio01 <- bioclim$wc2.1_10m_bio_1
bioclim_bio01

# plot
plot(bioclim_bio01, col = viridis::viridis(10))

# nomes
names(dem_rc)

# renomear
names(dem_rc) <- "elevacao"

# nomes
names(dem_rc)

# nomes
names(bioclim)

# renomear
names(bioclim) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))

# nomes
names(bioclim)

# media de todas as celulas de altitude
raster::cellStats(x = dem_rc, stat = mean)

# media de todas as celulas de cada camada bioclimatica
raster::cellStats(x = bioclim, stat = mean)

# frequencia das celulas
raster::freq(x = dem_rc) %>% head()

# frequencia das celulas
raster::freq(x = bioclim[[1]]) %>% head()

# soma
dem_rc2 <- dem_rc + dem_rc
dem_rc2

# plot
plot(dem_rc2, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# log10
dem_rc_log10 <- log10(dem_rc)
dem_rc_log10

# plot
plot(dem_rc_log10, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# upper 600
dem_rc_up_600 <- dem_rc < 500
dem_rc_up_600

prod(1, 2)

produto <- function(x, y){
  
  return(x*y)
  
}

produto(1, 2)

# plot
plot(dem_rc_up_600, col = viridis::viridis(2))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# produto dos pixel - calc
dem_rc_prod <- raster::calc(x = dem_rc, fun = function(x){x * x})
dem_rc_prod 

# plot
plot(dem_rc_prod, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# matriz de reclassificacao
rcl  <- matrix(
  c(400, 600, 1, 
    600, 800, 2, 
    800, 1000, 3), 
  ncol = 3, byrow = TRUE)
rcl

rcl  <- matrix(
  c(400, 600, 1, 
    600, 800, 2, 
    800, 1000, 3), 
  ncol = 3, byrow = TRUE)
rcl


# reclassificao
dem_rc_rcl <- raster::reclassify(x = dem_rc, rcl = rcl)
dem_rc_rcl

# plot
plot(dem_rc_rcl, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# janela movel
dem_rc_focal_sd <- raster::focal(x = dem_rc, w = matrix(data = 1, nrow = 3, ncol = 3), fun = sd)
dem_rc_focal_sd

# declividade
dem_rc_dec <- raster::terrain(x = dem_rc, opt = "slope", unit = "degrees")
dem_rc_dec

# plot
plot(dem_rc_focal_sd, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# plot
plot(dem_rc_dec, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# estatistica zonal
dem_rc_zonal <- data.frame(raster::zonal(dem_rc, dem_rc_rcl, fun = "summary"))
dem_rc_zonal

# estatistica zonal
colnames(dem_rc_zonal) <- c("zona", "min", "1qt", "mediana", "media", "3qt", "max")
dem_rc_zonal

# reclassificacao
dem_rc_abaixo_500 <- raster::calc(x = dem_rc, fun = function(x) ifelse(x < 500, 1, NA))
dem_rc_abaixo_500

# plot
plot(dem_rc_abaixo_500, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# distancia euclideana - demora uns 15 a 20 segundos!!!
dem_rc_global_dist <- raster::distance(dem_rc_abaixo_500)
dem_rc_global_dist

# plot
plot(dem_rc_global_dist, col = viridis::viridis(10))
plot(dem_rc_abaixo_500, add = TRUE, col = "white", legend = FALSE)
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# agregacao
# resolucao
res(dem_rc_utm23s)[1]

# agregacao - aumentar o tamanho do pixel
dem_rc_utm23s_agre_media <- raster::aggregate(x = dem_rc_utm23s, fact = 10, fun = "mean")
dem_rc_utm23s_agre_media

# plot
plot(dem_rc_utm23s_agre_media, col = viridis::viridis(10))
plot(rc_2020_utm23s$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# desagregacao
# resolucao
res(dem_rc_utm23s)[1]

# desagregacao - diminuir o tamanho do pixel
dem_rc_utm23s_dis_bil <- raster::disaggregate(dem_rc_utm23s, fact = 2, fun = "bilinear")
dem_rc_utm23s_dis_bil

# plot
plot(dem_rc_utm23s_dis_bil, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# reamostragem
st_rc <- raster::resample(x = bioclim$bio01, y = dem_rc, method = "bilinear")
st_rc

# plot
plot(st_rc, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# 7. interacoes raster vetor --------------------------------------------

# crop - ajustar da extensao
dem_rc_crop <- raster::crop(dem, rc_2020)
dem_rc_crop

# plot
plot(dem_rc_crop, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# mask - ajuste o limite
dem_rc_mask <- raster::mask(dem, rc_2020)
dem_rc_mask

# plot
plot(dem_rc_mask, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# crop e mask - ajuste da extensao e do limite
dem_rc_crop_mask <- dem %>% 
  raster::crop(rc_2020) %>% 
  raster::mask(rc_2020)
dem_rc_crop_mask

# plot
plot(dem_rc_crop_mask, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# crop and mask inverse - adjust extension and limit
dem_rc_crop_mask_inv <- dem %>% 
  raster::crop(rc_2020) %>% 
  raster::mask(rc_2020, inverse = TRUE)
dem_rc_crop_mask_inv

# plot
plot(dem_rc_crop_mask_inv, col = viridis::viridis(10))
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)


# importar pontos
rc_nas <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_NASCENTES.shp"), quiet = TRUE) %>% 
  sf::st_transform(crs = 4326)
rc_nas

plot(rc_nas[1], pch = 20, col = "blue", main = NA, axes = TRUE, graticule = TRUE)

# extracao
rc_nas_ele <- rc_nas %>% 
  dplyr::mutate(elev = raster::extract(x = dem_rc, y = .))

# plot
plot(rc_nas_ele["elev"], pch = 20, main = NA, axes = TRUE, graticule = TRUE)

# histograma
rc_nas_ele %>% 
  dplyr::pull(elev) %>% 
  hist(col = "steelblue", border = "white", main = NA, xlab = "Elevação (m)", ylab = "Frequência")

# zonal statistics
# buffers
set.seed(42)
rc_nas_buf <- rc_nas %>% 
  dplyr::sample_n(10) %>% 
  sf::as_Spatial() %>% 
  raster::buffer(width = 1000, dissolve = FALSE) %>% 
  sf::st_as_sf()
rc_nas_buf

# plot
plot(rc_nas_buf$geometry, col = adjustcolor("steelblue", .7), pch = 20, main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# zonal statistics
raster::extract(x = dem_rc, y = rc_nas_buf, fun = mean, na.rm = TRUE, df = TRUE)

# zonal statistics
rc_nas_buf <- rc_nas_buf %>% 
  dplyr::mutate(elev_mean = raster::extract(x = dem_rc_rcl, y = rc_nas_buf, fun = freq, na.rm = TRUE))
rc_nas_buf

# plot
plot(rc_nas_buf["elev_mean"], pch = 20, main = NA, axes = TRUE, graticule = TRUE)

# 8. Conversao raster-vetor --------------------------------------------

# importar pontos
rc_nas <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_NASCENTES.shp"), quiet = TRUE)
rc_nas

# rasterizar pontos
rc_nas_rasterizacao <- raster::rasterize(x = rc_nas, y = dem_rc_utm23s_agre_media,
                                         field = 1)

# plot
plot(rc_nas_rasterizacao, col = viridis::viridis(10))
plot(rc_nas$geometry, pch = 20, cex = .5, col = adjustcolor("gray", .5), add = TRUE)

# importar linhas
rc_hid_simp <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_RIOS_SIMPLES.shp"), quiet = TRUE) %>% 
  sf::st_simplify(x = ., dTolerance = 1000)
rc_hid_simp

# rasterizar linhas - demora uns 10 segundos!!!
rc_hid_rasterizacao <- raster::rasterize(x = rc_hid_simp,
                                         y = dem_rc_utm23s_agre_media,
                                         field = 1)

# plot
plot(rc_hid_rasterizacao, col = viridis::viridis(10))
plot(rc_hid_simp$geom, col = "gray", add = TRUE)

# importar poligonos
rc_cob <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_USO.shp"), quiet = TRUE) %>% 
  dplyr::mutate(classe = as.factor(CLASSE_USO))

# rasterizar poligonos - demora uns 5 segundos!!!
rc_cob_rasterizacao <- raster::rasterize(x = rc_cob, y = dem_rc_utm23s_agre_media, field = "classe")

# plot
plot(rc_cob_rasterizacao, col = viridis::viridis(10))
plot(rc_cob$geom, add = TRUE)

# package fasterize
# install.packages("fasterize")
library(fasterize)

# rasterizacao com fasterize
rc_cob_fast <- fasterize::fasterize(sf = rc_cob, raster = dem_rc_utm23s_agre_media, field = "classe")
rc_cob_fast

# plot
plot(rc_cob_fast, col = viridis::viridis(10))
plot(rc_cob$geom, add = TRUE)

# vetorizacao de pontos
dem_rc_utm23s_agre_media_pontos <- raster::rasterToPoints(dem_rc_utm23s_agre_media, spatial = TRUE) %>% 
  sf::st_as_sf()
dem_rc_utm23s_agre_media_pontos

# plot
plot(dem_rc_utm23s_agre_media, col = viridis::viridis(10, alpha = .8))
plot(dem_rc_utm23s_agre_media_pontos, pch = 20, cex = .7, main = FALSE, add = TRUE)
plot(rc_2020_utm23s$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# vetorizacao de linhas
dem_rc_utm23s_agre_media_linhas <- raster::rasterToContour(x = dem_rc_utm23s_agre_media) %>% 
  sf::st_as_sf()
dem_rc_utm23s_agre_media_linhas

# plot
plot(dem_rc_utm23s_agre_media, col = viridis::viridis(10, alpha = .8))
contour(dem_rc_utm23s_agre_media, levels =  seq(500, 900, by = 50),
        col = "white", pch = 20, lwd = 1.5, labcex = 2, main = FALSE, add = TRUE)
plot(rc_2020_utm23s$geom, col = NA, border = "red", lwd = 2, add = TRUE)

# vetorizacao de poligonos
rc_cob_rasterizacao_poligonos <- raster::rasterToPolygons(rc_cob_rasterizacao, digits = 12) %>% 
  sf::st_as_sf()
rc_cob_rasterizacao_poligonos

# plot
plot(rc_cob_rasterizacao, col = viridis::viridis(10))
plot(rc_cob_rasterizacao_poligonos$geometry, col = NA, border = "gray", lwd = 1, main = FALSE, add = TRUE)

# vetorizacao de poligonos dissolvendo
rc_cob_rasterizacao_poligonos_dissolvidos <- raster::rasterToPolygons(rc_cob_rasterizacao, dissolve = TRUE) %>% 
  sf::st_as_sf()
rc_cob_rasterizacao_poligonos_dissolvidos

# plot
plot(rc_cob_rasterizacao, col = viridis::viridis(10))
plot(rc_cob_rasterizacao_poligonos_dissolvidos$geometry, col = NA, border = "gray", lwd = 1, main = FALSE, add = TRUE)

# 9. indices espectrais --------------------------------------------------

# rio claro
landsat_rc

# plot - cores naturais
plotRGB(landsat_rc, r = 4, g = 3, b = 2, stretch = "hist")

# plot - falsa cor infravermelho
plotRGB(landsat_rc, r = 5, g = 4, b = 3, stretch = "hist")

# package RStoolbox
# install.packages("RStoolbox")
library(RStoolbox)

# ndvi
landsat_rc_ndvi <- RStoolbox::spectralIndices(img = landsat_rc, red = 4, nir = 5, indices = "NDVI")
landsat_rc_ndvi

# plot
plot(landsat_rc_ndvi)

# 10. exportar dados matriciais ------------------------------------------

# diretorio
dir.create(here::here("03_dados", "raster", "exportados"))

# exportar raster layer
raster::writeRaster(dem_rc, 
                    filename = here::here("03_dados", "raster", "exportados", "elevation_rio_claro"),
                    format = "GTiff",
                    datatype = "INT1U",
                    options = c("COMPRESS=DEFLATE", "TFW=YES"),
                    progress = "text",
                    overwrite = TRUE)

# exportar raster stack ou brick
raster::writeRaster(x = bioclim[[1:3]], 
                    filename = here::here("03_dados", "raster", "exportados", "bioclim"),
                    # bylayer = TRUE, 
                    format = "GTiff",
                    datatype = "INT2S",
                    options = c("COMPRESS=DEFLATE", "TFW=YES"),
                    progress = "text",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------