# Packages ----

library(tidyverse)
library(sf)
library(elevatr)
library(rayshader)
library(MetBrewer)
library(NatParksPalettes)


# Données ----


parcs <- st_read("geo/CDDA_2021_v01_public.gpkg")


cevennes <- parcs |> 
  filter(str_detect(siteName, "Cévennes"))


# Limites des 5 massifs
load("French National Parks/Cevennes/massifs.RData")


# Création d'un rectangle autour du parc pour avoir toute la zone
# Et étendu plus large pour que le parc tienne dans le cercle

limites <- st_bbox(st_buffer(cevennes, dist = 25000))

quad_cevennes <- matrix(
  c(
    limites[[1]], limites[[2]],
    limites[[1]], limites[[4]],
    limites[[3]], limites[[4]],
    limites[[3]], limites[[2]],
    limites[[1]], limites[[2]]
  ),
  ncol = 2,
  byrow = TRUE
) |>
  list() |> 
  st_polygon() |> 
  st_sfc(crs = "WGS84")


# Raster
mat <- get_elev_raster(quad_cevennes, z = 10, clip = "location") |> 
  raster_to_matrix()

wr <- nrow(mat) / max(c(nrow(mat),ncol(mat)))
hr <- ncol(mat) / max(c(nrow(mat),ncol(mat)))



# Couleurs ----



colors <- natparks.pals("Acadia")[5:9]

couleurs_massifs <- met.brewer("Klimt")[c(1,2,4,5,6)]


# Plot 3D ----

# Bien utiliser le pipe %>% sinon cela ne fonctionne pas toujours

rgl::rgl.close()

mat %>%
  height_shade(texture = grDevices::colorRampPalette(colors, bias = .7)(256)) %>%
  add_overlay(generate_polygon_overlay(massifs, 
                                       palette = couleurs_massifs,
                                       linewidth=NA,
                                       heightmap = mat,
                                       extent = raster::extent(limites)),
              alphalayer=.8) %>%
  plot_3d(heightmap = mat, 
          solid = FALSE,
          zscale = 3,
          shadow = F,
          windowsize = c(800*wr,800*hr), 
          phi = 45, 
          zoom = .6, 
          theta = 20,
          baseshape = "circle",
          background = "white") 



rgl::rgl.close()




# Render ----


render_highquality(
  "French National Parks/Cevennes/cevennes.png",
  samples = 300,
  parallel = TRUE,
  light = FALSE,
  interactive = FALSE,
  preview = FALSE,
  environment_light = "hdr/phalzer_forest_01_4k.hdr",
  intensity_env = 1.25,
  rotate_env = 120,
  width = round(6000 * wr), height = round(6000 * hr)
)


# Retouches et annotations faites dans Photoshop