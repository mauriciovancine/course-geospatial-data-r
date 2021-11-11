#' ---
#' title: aula 07 - estrutura e manejo de dados vetoriais 
#' author: mauricio vancine
#' date: 2021-10-25
#' ---
 
# packages ----------------------------------------------------------------
library(tidyverse)
library(sp)
library(sf)
library(geobr)
library(rnaturalearth)

# topics ------------------------------------------------------------------

# 1 principais pacotes
# 2 geometrias sf
# 3 classes sf
# 4 importar dados vetoriais
# 5 descricao de objetos sf
# 6 converter dados para sf
# 7 converter crs
# 8 operacoes de objetos sf
# 9 exportar dados vetoriais

# 1. princiapsi pacotes -----------------------------------------------------
# sp
# install.packages("sp")
library(sp)

# sf
# install.packages("sf")
library(sf)

# 3. classes sf ----------------------------------------------------------

# simple feature geometries (sfg)
# simple
# sf::st_point()
# sf::st_linestring()
# sf::st_polygon()

# multi
# sf::st_multipoint()
# sf::st_multilinestring()
# sf::st_multipolygon()

# collections
# sf::st_geometrycollection()

# vector - point
vec <- c(5, 2)
vec

po <- sf::st_point(vec)
po

# plot
dev.off()
plot(po, pch = 20, cex = 4, axes = TRUE, graticule = TRUE)

# matrix - multipoint
multipoint_matrix <-  rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
multipoint_matrix

po_mul <- sf::st_multipoint(multipoint_matrix)
po_mul

# plot
plot(po_mul, pch = 20, cex = 4, axes = TRUE, graticule = TRUE)

# matrix - linestring
multipoint_matrix <- rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
multipoint_matrix

lin <- sf::st_linestring(multipoint_matrix)
lin

# plot
plot(lin, lwd = 2, axes = TRUE, graticule = TRUE)

# list - polygon
polygon_list <- list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon_list

pol <- sf::st_polygon(polygon_list)
pol

# plot
plot(pol, col = "gray", axes = TRUE, graticule = TRUE)

# simple feature columns (sfc)
# sf::st_sfc()
# sf::st_geometry_type()
# sf::st_crs()

# sfc point
point1 <- sf::st_point(c(5, 2))
point1

point2 <- sf::st_point(c(1, 3))
point2

points_sfc <- sf::st_sfc(point1, point2)
points_sfc

sf::st_geometry_type(points_sfc)

# plot
plot(points_sfc, pch = 20, cex = 4, axes = TRUE, graticule = TRUE)

# sfc geometry
po_pol_sfc <- sf::st_sfc(po, pol)
po_pol_sfc

sf::st_geometry_type(po_pol_sfc)

# plot
plot(po_pol_sfc, pch = 20, cex = 4, lwd = 2, col = "gray", axes = TRUE, graticule = TRUE)

# epgs definition
points_sfc_wgs <- sf::st_sfc(point1, point2, crs = 4326)
points_sfc_wgs

sf::st_crs(points_sfc_wgs)

# proj4string definition
points_sfc_wgs <- sf::st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")
points_sfc_wgs

sf::st_crs(points_sfc_wgs)

# plot
plot(points_sfc_wgs, pch = 20, cex = 4, axes = TRUE, graticule = TRUE)

# class sf
rc_point <- sf::st_point(c(-47.57,-22.39))         # sfg object
rc_point

rc_geom <- sf::st_sfc(rc_point, crs = 4326)        # sfc object
rc_geom

rc_attrib <- data.frame(                       # data.frame object
  name = "Rio Claro",
  temperature = 19,
  date = as.Date("2020-10-13")
)
rc_attrib

rc_sf <- sf::st_sf(rc_attrib, geometry = rc_geom)  # sf object
rc_sf

class(rc_sf)

# plot
plot(rc_sf[1], pch = 20, cex = 4, axes = TRUE, graticule = TRUE) # rc_sf[1] - plotar a primeira coluna

# plot
plot(rc_sf[2], pch = 20, cex = 4, axes = TRUE, graticule = TRUE) # rc_sf[2] - plotar a segunda coluna

# plot
plot(rc_sf$geometry, pch = 20, cex = 4, axes = TRUE, graticule = TRUE) # rc_sf$geometry - plotar apenas a geometria

# 4. importar dados ------------------------------------------------------

# create directory
dir.create(here::here("03_dados", "vetor"))

# increase time to download
options(timeout = 600)

# download points
for(i in c(".dbf", ".prj", ".shp", ".shx")){
  download.file(url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/HIDROGRAFIA/SP_3543907_NASCENTES", i),
                destfile = here::here("03_dados", "vetor", paste0("SP_3543907_NASCENTES", i)), mode = "wb")
}

# download lines
for(i in c(".dbf", ".prj", ".shp", ".shx")){
  download.file(url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/HIDROGRAFIA/SP_3543907_RIOS_SIMPLES", i),
                destfile = here::here("03_dados", "vetor", paste0("SP_3543907_RIOS_SIMPLES", i)), mode = "wb")
}

# download polygons
for(i in c(".dbf", ".prj", ".shp", ".shx")){
  download.file(url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/USO/SP_3543907_USO", i),
                destfile = here::here("03_dados", "vetor", paste0("SP_3543907_USO", i)), mode = "wb")
}

# importar points
setwd("/home/mude/data/github/course-geospatial-data-r")
rc_nas <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_NASCENTES.shp"), quiet = FALSE)
rc_nas

# plot
plot(rc_nas$geometry, pch = 20, col = "blue", main = NA, axes = TRUE, graticule = TRUE)

# import lines
rc_hid <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_RIOS_SIMPLES.shp"), quiet = TRUE)
rc_hid

# plot
plot(rc_hid$geometry, col = "steelblue", main = NA, axes = TRUE, graticule = TRUE)

# import polygons - pode demorar alguns segundos
rc_cob <- sf::st_read(here::here("03_dados", "vetor", "SP_3543907_USO.shp"), quiet = TRUE)
rc_cob

# plot
plot(rc_cob$geometry, col = c("blue", "orange", "gray30", "forestgreen", "green"), main = NA, axes = TRUE, graticule = TRUE)

# import gps data
gps_gpx <- sf::read_sf(here::here("03_dados", "vetor", "waypoints.gpx"), layer = "waypoints")
gps_gpx

# plot
plot(gps_gpx$geometry, cex = 4, pch = 20, col = "red", main = NA, axes = TRUE, graticule = TRUE)

# import gps data
gps_kml <- sf::read_sf(here::here("03_dados", "vetor", "waypoints.kml"))
gps_kml

# plot
plot(gps_kml$geometry, cex = 4, pch = 20, col = "blue", main = NA, axes = TRUE, graticule = TRUE)

# importar tabela
si <- readr::read_csv(
  here::here("03_dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.csv"))
si

# converter para sf
si_ve <- si %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
si_ve

# plot
plot(si_ve$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE)

# import data from packages
# brazil 2020
br_2020 <- geobr::read_country(year = 2020)
br_2020

# plot
plot(br_2019$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# brazil 1872
br_1872 <- geobr::read_country(year = 1872)
br_1872

# plot
plot(br_1872$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# sao paulo municipalities
sp_mun_2020 <- geobr::read_municipality(code_muni = "SP", year = 2020)
sp_mun_2020

# plot
plot(sp_mun_2020$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# rio claro
rc_2020 <- geobr::read_municipality(code_muni = 3543907, year = 2020)
rc_2020

# plot
plot(rc_2020$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# biomes
bi_2004 <- geobr::read_biomes(year = 2004)
bi_2004

bi_2019 <- geobr::read_biomes(year = 2019)
bi_2019

# plot
plot(bi_2004$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(bi_2019$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# list all datasets available in the geobr package
geobr::list_geobr()

# south america
sa <- rnaturalearth::ne_countries(scale = "small", continent = "South America", returnclass = "sf")
sa

# plot
plot(sa$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# 5. descricao de objetos sf ---------------------------------------------
# rio claro
rc_2020

# geometry type
sf::st_geometry_type(rc_2020)

# extention
sf::st_bbox(rc_2020)

# coordinate reference system
sf::st_crs(rc_2020)

# acessar a tabela de atributos
rc_2020_tab <- sf::st_drop_geometry(rc_2020)
rc_2020_tab

# classe
class(rc_2020_tab)

# 6. converter dados sf --------------------------------------------------
# countries sp
co110_sp <- rnaturalearth::countries110
co110_sp

# countries sf
co110_sf <- sf::st_as_sf(co110_sp)
co110_sf

# plot
plot(co110_sf$geometry, col = "gray", main = NA)

# countries sp
co110_sp <- sf::as_Spatial(co110_sf)
co110_sp

# 7. conversao do crs --------------------------------------------------

# crs local
# converter sistema de coordenadas
rc_2020_sirgas2000_utm23s <- sf::st_transform(rc_2020, crs = 31983)
rc_2020_sirgas2000_utm23s

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", graticule = TRUE, axes = TRUE)

# converter sistema de coordenadas e datum
rc_2020_wgs84_utm23s <- sf::st_transform(rc_2020, crs = 32723)
rc_2020_wgs84_utm23s

# plot
plot(rc_2020_wgs84_utm23s$geom, col = "gray", graticule = TRUE, axes = TRUE)

# converter datum
rc_2020_wgs84_gcs <- sf::st_transform(rc_2020, crs = 4326)
rc_2020_wgs84_gcs

# plot
plot(rc_2020_wgs84_gcs$geom, col = "gray", graticule = TRUE, axes = TRUE)

# crs global
# paises - WGS84/GCS
co110_sf

# plot
plot(co110_sf$geometry, col = "gray", graticule = TRUE)

# projecao mollweide 
co110_sf_moll <- sf::st_transform(co110_sf, crs = "+proj=moll")
co110_sf_moll

# plot
plot(co110_sf_moll$geometry, col = "gray", graticule = TRUE)

# projecao lambert
co110_sf_laea <- sf::st_transform(co110_sf, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")
co110_sf_laea

# plot
plot(co110_sf_laea$geometry, col = "gray", graticule = TRUE)

# 8. operacoes de atributos ----------------------------------------------

# 1. operacoes de atributos ----

# 1.1 filtro
# filtro
rc_cob_floresta <- rc_cob %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")
rc_cob_floresta

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_cob_floresta$geometry, col = "forestgreen", add = TRUE)

# 1.2 juncao
# dados
da_classes <- tibble::tibble(CLASSE_USO = rc_cob$CLASSE_USO, 
                             classe = c(NA, "antropico", "edificado", "floresta", "silvicultura"))
da_classes

# juncao
rc_cob_classes <- dplyr::left_join(rc_cob, da_classes, by = "CLASSE_USO") %>% 
  sf::st_drop_geometry()
rc_cob_classes

# 1.3 agregacao
rc_nas_n <- rc_nas %>% 
  dplyr::group_by(MUNICIPIO) %>% 
  dplyr::summarise(n = n())
rc_nas_n

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_nas_n$geometry, pch = 20, col = "blue", add = TRUE)

# 1.4 Manipulação da tabela de atributos
# criar coluna
rc_cob_cob_col_area <- rc_cob %>% 
  dplyr::mutate(classe_area = paste0(CLASSE_USO, " (", AREA_HA, " ha)")) %>% 
  sf::st_drop_geometry()
rc_cob_cob_col_area

# comprimento das linhas
rc_hid_comp <- rc_hid %>% 
  dplyr::mutate(comprimento = sf::st_length(.))
rc_hid_comp

# area das poligonos
rc_cob_area <- rc_cob %>% 
  dplyr::mutate(area_m2 = sf::st_area(.))
rc_cob_area

# 2. operacoes espaciais ----
# 2.1 filtro espacial
# filtro espacial
sf::st_intersects(x = rc_nas, y = rc_cob_floresta)

# filtro espacial - interno
sf::st_intersects(x = rc_nas, y = rc_cob_floresta, sparse = FALSE)

rc_nas_floresta_int <- rc_nas %>% 
  dplyr::filter(sf::st_intersects(x = ., y = rc_cob_floresta, sparse = FALSE))
rc_nas_floresta_int

# filtro espacial com [] - interno
rc_nas_floresta_int <- rc_nas[rc_cob_floresta, ]
rc_nas_floresta_int

rc_cob_floresta_nas_int <- st_cast(rc_cob_floresta, "POLYGON")[rc_nas, ]
rc_cob_floresta_nas_int

rc_cob_floresta_nas_int$geometry %>% plot

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_cob_floresta_nas_int$geometry, col = "forestgreen", add = TRUE)
plot(rc_cob_floresta$geometry, col = "green", pch = 20, cex = 1, add = TRUE)

# filtro espacial - externo
rc_nas_floresta_ext <- rc_nas %>% 
  dplyr::filter(sf::st_disjoint(x = rc_nas, y = rc_cob_floresta, sparse = FALSE))
rc_nas_floresta_ext

# filtro espacial com [] - externo
rc_nas_floresta_ext <- rc_nas[rc_cob_floresta, , op = st_disjoint]

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_cob_floresta$geometry, col = "forestgreen", add = TRUE)
plot(rc_nas_floresta_ext$geometry, col = "steelblue", pch = 20, cex = 1, add = TRUE)

# 2.2 juncao espacial
# juncao espacial
rc_nas_cob_jun <- rc_nas %>% 
  sf::st_join(x = ., y = rc_cob) %>% 
  dplyr::group_by(CLASSE_USO) %>% 
  dplyr::summarise(n = n())
rc_nas_cob_jun

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_nas_cob_jun[1], col = c("blue", "orange", "gray30", "forestgreen", "green"), pch = 20, 
     legend = TRUE, add = TRUE)
legend(x = 213000, y = 7515000, pch = 15, cex = 1, pt.cex = 2.5, 
       legend = (rc_nas_cob_jun$CLASSE_USO), 
       col = c("blue", "orange", "gray30", "forestgreen", "green"))

# 2.3 agregacao espacial
# agregacao espacial
rc_cob_nas_agre <- rc_nas %>% 
  aggregate(x = ., by = rc_cob, FUN = length)
rc_cob_nas_agre

# plot
plot(rc_cob_nas_agre[1], axes = TRUE, graticule = TRUE, main = NA)

# 2.4 distancia espacial
# distancia
rc_nas_dist_flo <- rc_nas %>% 
  dplyr::mutate(dist_flo = sf::st_distance(rc_nas, rc_cob_floresta))
rc_nas_dist_flo

# plot
plot(rc_nas_dist_flo[7], pch = 20, legend = TRUE)

# 3. operacoes geometricas ----
# 3.1 simplificacao
# simplificacao
rc_hid_simplificado <- sf::st_simplify(x = rc_2020_sirgas2000_utm23s, 
                                       dTolerance = 1)
rc_hid_simplificado

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_hid$geometry, col = "steelblue", lwd = 2, add = TRUE)
plot(rc_hid_simplificado$geometry, col = adjustcolor("black", .7))

# 3.2 centroides
# centroides
rc_2020_sirgas2000_utm23s_cent <- sf::st_centroid(rc_2020_sirgas2000_utm23s)
rc_2020_sirgas2000_utm23s_cent

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_cent$geom, cex = 3, pch = 20, add = TRUE)

# 3.3 pontos aleatorios
# fixar amostragem
set.seed(42)

# pontos aleatorios
rc_2020_sirgas2000_utm23s_pontos_aleatorios <- sf::st_sample(rc_2020_sirgas2000_utm23s, size = 30)
rc_2020_sirgas2000_utm23s_pontos_aleatorios

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios, pch = 20, add = TRUE)

# 3.4 Buffer
# buffer
rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer <- sf::st_buffer(x = rc_2020_sirgas2000_utm23s_pontos_aleatorios, 
                                                                    dist = 1000)
rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer, col = NA, lwd = 2, border = "red", add = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios, pch = 20, cex = 1, add = TRUE)

# 3.5 poligono convexo
# poligono convexo
rc_2020_sirgas2000_utm23s_convexo <- rc_2020_sirgas2000_utm23s_pontos_aleatorios %>% 
  sf::st_union() %>% 
  sf::st_convex_hull()
rc_2020_sirgas2000_utm23s_convexo

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_convexo, col = NA, lwd = 2, border = "red", add = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios, pch = 20, cex = 1, add = TRUE)

# 3.6 Poligonos de voronoi
# poligono de voronoi
rc_2020_sirgas2000_utm23s_voronoi <- rc_2020_sirgas2000_utm23s_pontos_aleatorios %>% 
  sf::st_union() %>% 
  sf::st_voronoi()
rc_2020_sirgas2000_utm23s_voronoi

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_voronoi, col = NA, lwd = 2, border = "red", add = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios, pch = 20, cex = 1, add = TRUE)

# 3.7 quadriculas e hexagonos
# quadriculas
rc_2020_sirgas2000_utm23s_grid <- sf::st_make_grid(x = rc_2020_sirgas2000_utm23s, cellsize = 2000, what = "polygons") %>%
  sf::st_as_sf() %>%
  dplyr::filter(sf::st_intersects(x = ., y = rc_2020_sirgas2000_utm23s, sparse = FALSE))

# centroides das quadriculas
rc_2020_sirgas2000_utm23s_grid_cent <- rc_2020_sirgas2000_utm23s %>% 
  sf::st_make_grid(cellsize = 2000, what = "centers") %>%
  sf::st_as_sf() %>%
  dplyr::filter(sf::st_intersects(x = ., y = sf::st_union(rc_2020_sirgas2000_utm23s_grid), sparse = FALSE))

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_grid, col = NA, border = "red", lwd = 2, add = TRUE)
plot(rc_2020_sirgas2000_utm23s_grid_cent, pch = 20, add = TRUE)

# hexagonos
rc_2020_sirgas2000_utm23s_hex <- rc_2020_sirgas2000_utm23s %>% 
  sf::st_make_grid(cellsize = 2000, square = FALSE) %>% 
  sf::st_as_sf() %>%
  dplyr::filter(sf::st_intersects(x = ., y = rc_2020_sirgas2000_utm23s, sparse = FALSE))

# centroides de hexagonos
rc_2020_sirgas2000_utm23s_hex_cent <- rc_2020_sirgas2000_utm23s %>% 
  sf::st_make_grid(cellsize = 2000, square = FALSE, what = "centers") %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(sf::st_intersects(x = ., y = sf::st_union(rc_2020_sirgas2000_utm23s_hex), sparse = FALSE))

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_hex, col = NA, border = "red", lwd = 2, add = TRUE)
plot(rc_2020_sirgas2000_utm23s_hex_cent, pch = 20, add = TRUE)

# 3.8 uniao ("dissolver")
# uniao
rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer_uniao <- sf::st_union(rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer)

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer_uniao, col = adjustcolor("blue", .1), add = TRUE)

# 3.9 Recorte ("clipar")
# recorte - interseccao
rc_hid_interseccao <- sf::st_intersection(x = rc_hid, y = rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer_uniao)

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer_uniao, col = adjustcolor("blue", .1), add = TRUE)
plot(rc_hid_interseccao$geometry, col = "blue", add = TRUE)

# recorte - diferenca
rc_hid_diferenca <- sf::st_difference(x = rc_hid, y = rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer_uniao)

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_2020_sirgas2000_utm23s_pontos_aleatorios_buffer_uniao, col = adjustcolor("blue", .1), add = TRUE)
plot(rc_hid_diferenca$geometry, col = "blue", add = TRUE)

# 3.10 transformacoes de tipo
# transformacao de tipo
rc_cob_floresta_polygon <- rc_cob_floresta %>% 
  sf::st_cast("POLYGON") %>% 
  dplyr::mutate(area_ha_id = sf::st_area(.)/1e4 %>% round(2))
rc_cob_floresta_polygon

# plot
plot(rc_2020_sirgas2000_utm23s$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(rc_cob_floresta_polygon["area_ha_id"], col = viridis::viridis(100),  add = TRUE)

# 9. exportar dados vetoriais --------------------------------------------

# exportar o vetor de floresta na extensão esri shapefile
sf::st_write(obj = rc_cob_floresta_polygon, dsn = here::here("03_dados", "vetor", "rc_cob_floresta_polygon.shp"))

# exportar o vetor de floresta na extensão geopackage
sf::st_write(obj = rc_cob_floresta_polygon, dsn = here::here("03_dados", "vetor", "vetores.gpkg"), layer = "rc_cob_floresta_polygon")

# exportar o vetor da america do sul na extensão geopackage
sf::st_write(obj = sa, dsn = here::here("03_dados", "vetor", "vetores.gpkg"), layer = "south_america")

sf::st_write(obj = rc_hid_simplificado, dsn = here::here("03_dados", "vetor", "vetores.gpkg"), 
             layer = "rc_hid_diferenca")

# end ---------------------------------------------------------------------