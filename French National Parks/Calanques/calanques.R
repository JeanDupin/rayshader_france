# Packages ----

library(tidyverse)
library(sf)
library(elevatr)
library(rayshader)
library(MetBrewer)
library(NatParksPalettes)



# Données ----

ls_communes <- data.table::fread("Calanques/communes_13.csv",
                                 colClasses = c("Code" = "character"),
                                 select = "Code")

communes <- st_read("geo/communes_2021.shp") |> 
  filter(code %in% ls_communes$Code) |> 
  st_transform(crs = st_crs("WGS84"))



calanques <- st_read("geo/CDDA_2021_v01_public.gpkg") |> 
  filter(stringr::str_detect(siteName,"Calanques")) |> 
  st_union()


litoral <- st_union(calanques, communes)

ggplot() +
  geom_sf(data = litoral)





# Limites ----

limites <- st_bbox(litoral)

quad_litoral <- matrix(
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
mat <- get_elev_raster(quad_litoral, z = 10, clip = "location") |> 
  raster_to_matrix()

wr <- nrow(mat) / max(c(nrow(mat),ncol(mat)))
hr <- ncol(mat) / max(c(nrow(mat),ncol(mat)))



# Couleurs ----

glacier <- natparks.pals("Glacier", n = 6)
colorspace::swatchplot(glacier)


acadia <- natparks.pals("Acadia")[7:9]
colorspace::swatchplot(acadia)

olympic <- natparks.pals("Olympic")[6:9]
colorspace::swatchplot(olympic)


# Plot 3D ----

# Bien utiliser le pipe %>% sinon cela ne fonctionne pas toujours

rgl::rgl.close()


fond_marin = height_shade(mat,
                    texture = grDevices::colorRampPalette(glacier,
                                                          bias = .3)(256),
                    range = c(-3900, -0.1))


mat %>%
  height_shade(texture = grDevices::colorRampPalette(olympic)(256),
               range = c(0, 1200)) %>%
  add_overlay(generate_altitude_overlay(fond_marin, mat, 0, 0)) %>%
  plot_3d(heightmap = mat, 
          solid = FALSE,
          zscale = 5,
          shadow = FALSE,
          # shadowdepth = -3034,
          windowsize = c(800*wr,800*hr), 
          phi = 35, 
          zoom = .8, 
          theta = -10, 
          background = "white",
          baseshape = "circle")
  



rgl::rgl.close()



# Render ----


render_highquality(
  "Calanques/calanques.png",
  samples = 300,
  parallel = TRUE,
  light = FALSE,
  interactive = FALSE,
  preview = FALSE,
  environment_light = "hdr/phalzer_forest_01_4k.hdr",
  intensity_env = 1.25,
  rotate_env = -60,
  width = round(6000 * wr), height = round(6000 * hr)
)
