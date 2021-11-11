#' ---
#' title: aula 08 - Produção de mapas
#' author: mauricio vancine
#' date: 2020-10-23
#' ---

# packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(geobr)
library(rnaturalearth)
library(viridis)
library(wesanderson)
library(cptcity)
library(ggspatial)
library(ggmap)
library(tmap)
library(mapsf)
library(mapview)
library(plotly)
library(mapedit)

# topics ------------------------------------------------------------------

# 1. Elementos de um mapa
# 2. Pacotes para produção de mapas
# 3. Mapas estáticos
# 4. Mapas animados
# 5. Mapas interativos

# 1. principais elementos de um mapa -------------------------------------

# elementos 
# - mapa principal
# - mapa secundario
# - titutlo
# - legenda
# - gride de coordenadas
# - barra de escala
# - orientacao (norte)
# - descricao do src
# - informacao de origem~
# - outras informacoes

# 2. pacotes para producao de mapas --------------------------------------

# principais pacotes para producao de mapas no R
# - ggplot2
# - ggspatial
# - ggmap
# - tmap
# - mapsf
# - linemap
# - cartography
# - cartogram
# - googleway
# - rasterVis
# - mapview
# - leaflet
# - plotly
# - mapedit

# 3. mapas estaticos -----------------------------------------------------

# biomas
biomas <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome != "Sistema Costeiro")
biomas

# plot
plot(biomas)
plot(biomas$geom)

# plot
plot(biomas$geom, 
     col = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"),
     main = "Biomas do Brasil", axes = TRUE, graticule = TRUE)
legend(x = -72, y = -20, pch = 15, cex = 1, pt.cex = 2.5, legend = biomas$name_biome, 
       col = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"))

# rio claro
rc_2020 <- geobr::read_municipality(code_muni = 3543907, year = 2020, showProgress = FALSE) %>% 
  sf::st_transform(crs = 4326)
rc_2020

# importar raster
dem_rc <- raster::raster(here::here("03_dados", "raster", "srtm_27_17.tif")) %>% 
  raster::crop(rc_2020)
dem_rc

# plot
plot(dem_rc)
plot(rc_2020$geom, col = NA, border = "red", lwd = 2, add = TRUE)

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

# plot
plot(bioclim[[1:19]], col = viridis::viridis(10))

# diretorio
dir.create(here::here("03_dados", "mapas"))

# exportar mapa
png(filename = here::here("03_dados", "mapas", "mapa_biomas.png"), 
    width = 20, height = 20, units = "cm", res = 300)
plot(biomas$geom, 
     col = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"),
     main = "Biomas do Brasil", axes = TRUE, graticule = TRUE)
legend(x = -73, y = -20, pch = 15, cex = 1, pt.cex = 2.5, legend = biomas$name_biome, 
       col = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"))
dev.off()

# ggplot2 ----
# dados
ggplot() +
  geom_sf(data = biomas)

# cor e preenchimento
ggplot() +
  geom_sf(data = biomas, color = "black", fill = NA)

# cor e preenchimento
ggplot() +
  geom_sf(data = biomas, aes(color = name_biome), fill = NA)

# cor e preenchimento
ggplot() +
  geom_sf(data = biomas, aes(fill = name_biome), color = NA)

# definir cor
ggplot() +
  geom_sf(data = biomas, aes(fill = name_biome), color = NA) +
  scale_fill_manual(values = c("darkgreen", "orange", "orange4", 
                               "forestgreen", "yellow", "yellow3"))
# tema
ggplot() +
  geom_sf(data = biomas, aes(fill = name_biome), color = NA) +
  scale_fill_manual(values = c("darkgreen", "orange", "orange4", 
                               "forestgreen", "yellow", "yellow3")) +
  theme_bw()

# barra de escala e norte
ggplot() +
  geom_sf(data = biomas, aes(fill = name_biome), color = NA) +
  scale_fill_manual(values = c("darkgreen", "orange", "orange4", 
                               "forestgreen", "yellow", "yellow3")) +
  theme_bw(base_size = 15) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering)

# nomes e anotacoes
ggplot(data = biomas) +
  aes(fill = name_biome) +
  geom_sf(color = "black") +
  scale_fill_manual(values = c("darkgreen", "orange", "orange4", 
                               "forestgreen", "yellow", "yellow3")) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", label = "CRS: SIRGAS2000/Geo", x = -38, y = -31, size = 4) +
  annotate(geom = "text", label = "Fonte: IBGE (2019)", x = -39, y = -32.5, size = 4) +
  labs(title = "Biomas do Brasil", fill = "Legenda", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 15) +
  theme(legend.title = element_text(size = 15, face = "bold"),
        legend.position = c(.15, .25),
        legend.background = element_rect(colour = "black"))

# atribuicao
map_biomas_ggplot2 <- ggplot(data = biomas) +
  aes(fill = name_biome) +
  geom_sf(color = "black") +
  scale_fill_manual(values = c("darkgreen", "orange", "orange4", 
                               "forestgreen", "yellow", "yellow3")) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", label = "CRS: SIRGAS2000/Geo", x = -38, y = -31, size = 4) +
  annotate(geom = "text", label = "Fonte: IBGE (2019)", x = -39, y = -32.5, size = 4) +
  labs(title = "Biomas do Brasil", fill = "Legenda", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 15) +
  theme(legend.title = element_text(size = 15, face = "bold"),
        legend.position = c(.15, .25),
        legend.background = element_rect(colour = "black"))
map_biomas_ggplot2

# dados
dem_rc_da <- raster::rasterToPoints(dem_rc) %>% 
    tibble::as_tibble()
head(dem_rc_da)

# plot
map_dem_rc_ggplot2 <- ggplot() +
  geom_raster(data = dem_rc_da, aes(x = x, y = y, fill = srtm_27_17)) +
  geom_sf(data = rc_2020, col = "red", fill = NA, size = 1.3) +
  scale_fill_viridis_c() +
  coord_sf() +
  theme_bw(base_size = 15) +
  theme(legend.title = element_text(size = 15, face = "bold"),
        legend.position = c(.2, .2),
        legend.background = element_rect(colour = "black")) +
  annotation_scale(location = "br",
                   pad_x = unit(.5, "cm"), pad_y = unit(.7, "cm"),) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(.4, "cm"), pad_y = unit(1.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", label = "CRS: WGS84/Geo", x = -47.51, y = -22.53, size = 3) +
  labs(title = "Elevação de Rio Claro/SP", fill = "Elevação (m)", x = "Longitude", y = "Latitude")
map_dem_rc_ggplot2

# exportar
ggsave(
  filename = here::here("03_dados", "mapas", "mapa_biomas_ggplot2.png"),
  plot = map_biomas_ggplot2, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300
)

# exportar
ggsave(
  filename = here::here("03_dados", "mapas", "mapa_dem_rc_rc_ggplot2.png"), 
  plot = map_dem_rc_ggplot2, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300
)

# ggmap ----

# pacote
# install.packages("ggmap)
library(ggmap)

# limite
brasil <- c(left = -80, bottom = -35, right = -30, top = 5)
rio_claro <- c(left = -47.67, bottom = -22.48, right = -47.46, top = -22.32)

# plot
get_stamenmap(brasil, maptype = "terrain", zoom = 5) %>% 
  ggmap()

# plot
get_stamenmap(brasil, maptype = "toner", zoom = 5) %>% 
  ggmap()

# plot
get_stamenmap(brasil, maptype = "toner-lite", zoom = 5) %>% 
  ggmap()

# plot
get_stamenmap(brasil, maptype = "watercolor", zoom = 5) %>% 
  ggmap()

# plot
ggmap(get_stamenmap(rio_claro, maptype = "terrain", zoom = 12)) +
  geom_sf(data = rc_2020, col = "red", fill = NA, size = 1.3) +
  coord_sf()

# tmap ----

# poligonos
tm_shape(biomas) +
  tm_polygons()

# bordas
tm_shape(biomas) +
  tm_borders()

# prenchimento
tm_shape(biomas) +
  tm_fill()

# cores
tm_shape(biomas) +
  tm_fill(col = "name_biome", title = "Legenda")

# definir cores
tm_shape(biomas) +
  tm_fill(col = "name_biome", 
          pal = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"),
          title = "Legenda")

# gride coordenadas
tm_shape(biomas) +
  tm_fill(col = "name_biome", 
          pal = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"), 
          title = "Legenda") +
  tm_grid(lines = FALSE, 
          labels.format = list(big.mark = ""), 
          labels.rot = c(0, 90))

# barra de escala e norte
tm_shape(biomas) +
  tm_fill(col = "name_biome", 
          pal = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"), 
          title = "Legenda") +
  tm_grid(lines = FALSE, 
          labels.format = list(big.mark = ""), 
          labels.rot = c(0, 90)) +
  tm_compass() +
  tm_scale_bar()

# nomes
tm_shape(biomas) +
  tm_fill(col = "name_biome", 
          pal = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"), 
          title = "Legenda") +
  tm_grid(lines = FALSE, 
          labels.format = list(big.mark = ""), 
          labels.rot = c(0, 90)) +
  tm_compass() +
  tm_scale_bar() +
  tm_xlab("Longitude") +
  tm_ylab("Latitude") +
  tm_credits("CRS: SIRGAS2000/Geo", position = c(.63, .13)) +
  tm_credits("Fonte: IBGE (2019)", position = c(.63, .09))

# titulos
tm_shape(biomas) +
  tm_fill(col = "name_biome", 
          pal = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"), 
          title = "Legenda") +
  tm_graticules(lines = FALSE, 
                labels.format = list(big.mark = ""), 
                labels.rot = c(0, 90),
                labels.size = 1) +
  tm_compass(size = 3) +
  tm_scale_bar(size = 1) +
  tm_xlab("Longitude", size = 1.5) +
  tm_ylab("Latitude", size = 1.5) +
  tm_credits("CRS: SIRGAS2000/Geo", position = c(.63, .13), size = 1) +
  tm_credits("Fonte: IBGE (2019)", position = c(.63, .09), size = 1) +
  tm_layout(main.title = "Biomas do Brasil",
            main.title.position = c(.1, .95),
            main.title.size = 3,
            title.fontface = "bold",
            legend.frame = TRUE,
            legend.position = c("left", "bottom"),
            legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.text.size = 1)

# atribuicao
map_biomas_tmap <- tm_shape(biomas) +
  tm_fill(col = "name_biome", 
          pal = c("darkgreen", "orange", "orange4", "forestgreen", "yellow", "yellow3"), 
          title = "Legenda") +
  tm_graticules(lines = FALSE, 
                labels.format = list(big.mark = ""), 
                labels.rot = c(0, 90),
                labels.size = 1) +
  tm_compass(size = 3) +
  tm_scale_bar(size = 1) +
  tm_xlab("Longitude", size = 1.5) +
  tm_ylab("Latitude", size = 1.5) +
  tm_credits("CRS: SIRGAS2000/Geo", position = c(.63, .13), size = 1) +
  tm_credits("Fonte: IBGE (2019)", position = c(.63, .09), size = 1) +
  tm_layout(main.title = "Biomas do Brasil",
            main.title.position = c(.1, .95),
            main.title.size = 3,
            title.fontface = "bold",
            legend.frame = TRUE,
            legend.position = c("left", "bottom"),
            legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.text.size = 1)
map_biomas_tmap

# plot
map_dem_rc_tmap <- tm_shape(dem_rc) +
  tm_raster(title = "Legenda") +
  tm_shape(rc_2020) +
  tm_borders(col = "red", lwd = 2) +
  tm_graticules(lines = FALSE, 
                labels.format = list(big.mark = ""), 
                labels.rot = c(0, 90),
                labels.size = 1) +
  tm_compass(size = 3) +
  tm_scale_bar(size = 1) +
  tm_xlab("Longitude", size = 1.5) +
  tm_ylab("Latitude", size = 1.5) +
  tm_layout(main.title = "Elevação Rio Claro/SP",
            main.title.position = c(.1, .95),
            main.title.size = 3,
            title.fontface = "bold",
            legend.frame = TRUE,
            legend.position = c("left", "bottom"),
            legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.text.size = 1)
map_dem_rc_tmap

# plot
map_dem_rc_tmap <- tm_shape(dem_rc) +
  tm_raster(pal = wesanderson::wes_palette("Zissou1"), title = "Legenda") +
  tm_shape(rc_2020) +
  tm_borders(col = "red", lwd = 2) +
  tm_graticules(lines = FALSE, 
                labels.format = list(big.mark = ""), 
                labels.rot = c(0, 90),
                labels.size = 1) +
  tm_compass(size = 3) +
  tm_scale_bar(size = 1) +
  tm_xlab("Longitude", size = 1.5) +
  tm_ylab("Latitude", size = 1.5) +
  tm_layout(main.title = "Elevação Rio Claro/SP",
            main.title.position = c(.1, .95),
            main.title.size = 3,
            title.fontface = "bold",
            legend.frame = TRUE,
            legend.position = c("left", "bottom"),
            legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.text.size = 1)
map_dem_rc_tmap

# plot
map_dem_rc_tmap <- tm_shape(dem_rc) +
  tm_raster(pal = cptcity::cpt(pal = "gmt_GMT_dem4"), n = 20, title = "Legenda") +
  tm_shape(rc_2020) +
  tm_borders(col = "red", lwd = 2) +
  tm_graticules(lines = FALSE, 
                labels.format = list(big.mark = ""), 
                labels.rot = c(0, 90),
                labels.size = 1) +
  tm_compass(size = 3) +
  tm_scale_bar(size = 1) +
  tm_xlab("Longitude", size = 1.5) +
  tm_ylab("Latitude", size = 1.5) +
  tm_layout(main.title = "Elevação Rio Claro/SP",
            main.title.position = c(.1, .95),
            main.title.size = 3,
            title.fontface = "bold",
            legend.outside = TRUE,
            legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.text.size = 1)
map_dem_rc_tmap

# exportar
tmap::tmap_save(tm = map_biomas_tmap, 
                filename = here::here("03_dados", "mapas", "mapa_biomas_tmap.png"), 
                width = 20, 
                height = 20, 
                units = "cm", 
                dpi = 300)

# exportar
tmap::tmap_save(tm = map_dem_rc_tmap, 
                filename = here::here("03_dados", "mapas", "mapa_dem_rc_tmap.png"), 
                width = 20, 
                height = 20, 
                units = "cm", 
                dpi = 300)

# mapsf ----
# pacote
# install.packages("mapsf")
library(mapsf)

# sintaxe
# mf_map(x, var, type)

# plot
mf_map(x = biomas)

# plot
mf_map(x = biomas, var = "name_biome", type = "typo")

# plot
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -15))

# plot
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -15))
mf_title("Biomas do Brasil")
mf_credits("IBGE, 2019")
mf_scale()
mf_arrow()

# plot
mf_init(x = biomas, theme = "dark")
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -15),
       add = TRUE)
mf_title("Biomas do Brasil")
mf_credits("IBGE, 2019")
mf_scale()
mf_arrow()

# plot
mf_theme(bg = "lightblue", fg = "gray20")
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -15),
       add = TRUE)
mf_title("Biomas do Brasil")
mf_credits("IBGE, 2019")
mf_scale()
mf_arrow()

# plot
mf_init(x = biomas, theme = "dark")
mf_shadow(x = biomas, col = "gray10", add = TRUE)
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -15),
       add = TRUE)
mf_title("Biomas do Brasil")
mf_credits("IBGE, 2019")
mf_scale()
mf_arrow()

# plot
mf_init(x = biomas, theme = "dark") +
  mf_shadow(x = biomas, col = "gray10", add = TRUE)
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -15),
       add = TRUE) +
  mf_inset_on(x = "worldmap", pos = "topright")
mf_worldmap(biomas, col = "#0E3F5C")
mf_inset_off()
mf_title("Biomas do Brasil")
mf_credits("IBGE, 2019")
mf_scale(lwd = 2)
mf_arrow()

# export
mf_export(x = biomas, 
          filename = here::here("03_dados", "mapas", "mapa_biomas_mapsf.png"), 
          wi = 20, he = 20, un = "cm", res = 300)
mf_init(x = biomas, theme = "dark")
mf_shadow(x = biomas, col = "gray10", add = TRUE)
mf_map(x = biomas, var = "name_biome", type = "typo",
       pal = c("darkgreen", "orange", "orange4", 
               "forestgreen", "yellow", "yellow3"),
       leg_title = "Biomas",
       leg_pos = c(-70, -17),
       add = TRUE)
mf_inset_on(x = "worldmap", pos = "topright")
mf_worldmap(biomas, col = "#0E3F5C")
mf_inset_off()
mf_title("Biomas do Brasil")
mf_credits("IBGE, 2019")
mf_scale()
mf_arrow()
dev.off()

# rastervis ----
# pacote
# install.packages('rasterVis')
library(rasterVis)

# plot
levelplot(bioclim[[8:11]])

# plot
levelplot(bioclim, 
          layers = 1, 
          margin = list(FUN = "median"))

# plot
levelplot(bioclim, 
          layers = 4, 
          margin = list(FUN = "median"))

# export
png(filename = here::here("03_dados", "mapas", "mapa_bioclim_rastervis.png"), 
    wi = 20, he = 20, un = "cm", res = 300)
levelplot(bioclim, layers = 4, margin = list(FUN = "median"))
dev.off()

# 4. mapas animados ------------------------------------------------------

# dados
br_anos <- NULL
for(i in c(1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2001, 2010, 2020)){
  
  br_anos <- geobr::read_state(code_state = "all", year = i, showProgress = FALSE) %>% 
    sf::st_geometry() %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(year = i) %>% 
    dplyr::bind_rows(br_anos, .)
  
}

# numero de estados ao longo do tempo
br_anos$year %>% table()

# mapa facetado - demora uns 10 segundos
map_brasil_tmap <- tm_shape(br_anos) + 
  tm_polygons() + 
  tm_facets(by = "year", nrow = 4)
map_brasil_tmap

# mapa animado - pode demorar uns 10 segundos
map_brasil_tmap_ani <- tm_shape(br_anos) + 
  tm_polygons() + 
  tm_facets(along = "year", free.coords = FALSE)
map_brasil_tmap_ani

# exportar - pode demorar uns 10 segundos
tmap::tmap_animation(tm = map_brasil_tmap_ani, 
                     filename = here::here("03_dados", "mapas", "mapa_dem_rc_tmap_ani.gif"), 
                     delay = 30)

# 5. mapas interativos ---------------------------------------------------

# tmap ----
# mudar o modo de exibicao do tmap
tmap::tmap_mode(mode = "view")

# mapa interativo
map_biomas_tmap_int <- map_biomas_tmap
map_biomas_tmap_int

# mapa interativo
map_dem_rc_tmap_int <- map_dem_rc_tmap
map_dem_rc_tmap_int

# mapview ----
# pacote
# install.packge("mapview")
library(mapview)

# plot
map_dem_rc_mapview_int <- mapview::mapview(dem_rc, col.regions = viridis::viridis(100))
map_dem_rc_mapview_int

# exportar mapa tmap interativo
tmap::tmap_save(tm = map_dem_rc_tmap_int, 
                filename = here::here("dados", "mapas", "mapa_dem_rc_tmap_int.html"))

# leaflet ----
# pacote
# install.packge("leaflet")
library(leaflet)

# paleta de cores
pal <- colorNumeric(viridis::viridis(10), raster::values(dem_rc))

# mapa
map_dem_rc_leaflet_int <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addRasterImage(dem_rc, colors = pal, opacity = .8) %>%
  addLegend(pal = pal, values = raster::values(dem_rc), title = "Elevação (m)") %>% 
  addPolygons(data = rc_2020, col = "red", fill = NA)
map_dem_rc_leaflet_int

# exportar mapa tmap interativo
mapview::mapshot(x = map_dem_rc_leaflet_int, 
                 url = here::here("03_dados", "mapas", "mapa_dem_rc_leaflet_int.html"))

# plotly ----
# pacote
# install.packge("plotly")
library(plotly)

# plot
map_rc_2020_plotly_int <- plot_geo(rc_2020)
map_rc_2020_plotly_int

# plot
map_rc_2020_plotly_int <- ggplotly(
  ggplot() +
    geom_sf(data = rc_2020) +
    theme_bw(base_size = 16))
map_rc_2020_plotly_int

# plot
map_biomas_plotly_int <- ggplotly(
  ggplot(data = biomas) +
    aes(fill = name_biome) +
    geom_sf(color = "black") +
    scale_fill_manual(values = c("darkgreen", "orange", "orange4", 
                                 "forestgreen", "yellow", "yellow3")) +
    theme_bw(base_size = 16) +
    annotate(geom = "text", label = "CRS: SIRGAS2000/Geo", x = -38, y = -31, size = 2.5) +
    annotate(geom = "text", label = "Fonte: IBGE (2019)", x = -39, y = -32.5, size = 2.5) +
    labs(title = "Biomas do Brasil", fill = "Legenda", x = "Longitude", y = "Latitude"))
map_biomas_plotly_int

# exportar mapa tmap interativo
mapview::mapshot(x = map_biomas_plotly_int, 
                 url = here::here("03_dados", "mapas", "mapa_dem_rc_leaflet_int.html"))

# mapedit ----
# pacote
# install.packages("mapedit")
library(mapedit)

# criar
unesp <- mapedit::drawFeatures()
unesp

# editar
unesp_editado <- mapedit::editFeatures(unesp)
unesp_editado

# mapa
mapview::mapview(unesp_editado)

# muito obrigado pela paciencia e atencao =]

# end ---------------------------------------------------------------------