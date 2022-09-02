# Packages ----

library(tidyverse)
library(sf)
library(elevatr)
library(rayshader)
library(MetBrewer)
library(NatParksPalettes)
library(scico)


# Données ----


# Shapefile du parc des écrins
ecrins <- st_read("geo/CDDA_2021_v01_public.gpkg") |> 
  filter(stringr::str_detect(siteName,"Ecrins")) |> 
  st_union()



# Création d'un rectangle autour du parc pour avoir toute la zone
limites <- st_bbox(ecrins)

quad_ecrins <- matrix(
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
mat <- get_elev_raster(quad_ecrins, z = 10, clip = "location") |> 
  raster_to_matrix()

wr <- nrow(mat) / max(c(nrow(mat),ncol(mat)))
hr <- ncol(mat) / max(c(nrow(mat),ncol(mat)))



# Couleurs ----

monet <- met.brewer("Monet")
demuth <- met.brewer("Demuth")


colorspace::swatchplot(monet)
colorspace::swatchplot(demuth)


colors <- c(monet[1:3],
            demuth[6:9])

colorspace::swatchplot(colors)



# Plot 3D ----

# Bien utiliser le pipe %>% sinon cela ne fonctionne pas toujours

mat %>%
  height_shade(texture = grDevices::colorRampPalette(colors, bias = 1.6)(256)) %>%
  plot_3d(heightmap = mat, 
          solid = FALSE,
          zscale = 3,
          shadow = FALSE,
          windowsize = c(800*wr,800*hr), 
          phi = 70, 
          zoom = .8, 
          theta = 0, 
          background = "white",
          baseshape = "circle") 



rgl::rgl.close()



# Render ----



render_highquality(
  "Ecrins/ecrins.png",
  samples = 300,
  parallel = TRUE,
  light = FALSE,
  interactive = FALSE,
  preview = FALSE,
  environment_light = "hdr/phalzer_forest_01_4k.hdr",
  intensity_env = 1.25,
  rotate_env = -135,
  width = round(6000 * wr), height = round(6000 * hr)
)


# Retouches et annotations faites dans Photoshop

