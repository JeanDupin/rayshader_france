library(sf)
library(elevatr)
library(dplyr)


bfc <- st_read("geo/reg_francemetro_2021.shp") |> 
  filter(code == "27")



bfc_elev <- get_elev_raster(bfc, z = 10, clip = "location")

library(rayshader)

mat <- raster_to_matrix(bfc_elev)


# mat %>%
#   height_shade() %>%
#   plot_3d(heightmap = mat)
# 
# rgl::rgl.close()



library(MetBrewer)

colors <- met.brewer("Derain")


mat |> 
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) |> 
  plot_3d(heightmap = mat) 
