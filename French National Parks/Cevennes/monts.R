# FOND ----

# Ce fichier permet de créer les périmètres des 5 massifs du parc national
# des Cévennes.



# Packages ----

library(tidyverse)
library(sf)
library(data.table)


`%notin%` <- Negate(`%in%`)

# Données ----

parcs <- st_read("geo/CDDA_2021_v01_public.gpkg")


cevennes <- parcs |> 
  filter(str_detect(siteName, "Cévennes"))


communes <- st_read("geo/communes_2021.shp") |> 
  st_transform(st_crs("WGS84"))



# Massifs ----
# Le parc national est composé de 5 massifs. Le but est d'identifier leurs limites
# pour associer des couleurs.


coeur <- cevennes[2,]
aire <- cevennes[1,]


# Massif Aigoual

communes <- fread("French National Parks/Cevennes/communes_aigoual.csv",
                  encoding = "UTF-8",
                  colClasses = c("Code" = "character"))


aigoual <- rbind(
  st_read("geo/communes_2021.shp") |> 
    st_transform(st_crs("WGS84")) |> 
    filter(code %in% communes$Code & code %notin% c("30297","48020","48130")) |> 
    select(code),
  st_read("geo/communes_2021.shp") |> 
    st_transform(st_crs("WGS84")) |> 
    filter(code %in%  c("30297","48020","48130")) |> 
    st_intersection(coeur) |> 
    select(code)
) |> 
  st_union()


# Massif Vallées Cévenoles


communes <- fread("French National Parks/Cevennes/communes_vallees.csv",
                  encoding = "UTF-8",
                  colClasses = c("Code" = "character"))

vallees <- st_read("geo/communes_2021.shp") |>
  st_transform(st_crs("WGS84")) |>
  filter(code %in% communes$Code) |>
  select(code) |>
  st_union()


# Massif Mont Lozère

communes <- fread("French National Parks/Cevennes/communes_mont_lozere.csv",
                  encoding = "UTF-8",
                  colClasses = c("Code" = "character"))


lozere <- rbind(
  st_read("geo/communes_2021.shp") |> 
    st_transform(st_crs("WGS84")) |> 
    filter(code %in% communes$Code) |> 
    filter(code %notin% c("48027","48028")) |> 
    select(code),
  st_read("geo/communes_2021.shp") |>
    st_transform(st_crs("WGS84")) |>
    filter(code == "48028") |>
    st_intersection(coeur) |>
    st_cast("POLYGON") |>
    slice(1) |>
    select(code),
  st_read("geo/communes_2021.shp") |>
    st_transform(st_crs("WGS84")) |>
    filter(code == "48027") |>
    st_intersection(st_union(cevennes)) |>
    select(code)
) |> 
  st_union() |> 
  smoothr::fill_holes(units::set_units(100, km^2))


# Massif des Causses-Gorges


communes <- fread("French National Parks/Cevennes/communes_causses.csv",
                  encoding = "UTF-8",
                  colClasses = c("Code" = "character"))



causses <- rbind(
  st_read("geo/communes_2021.shp") |> 
    st_transform(st_crs("WGS84")) |> 
    filter(code %in% communes$Code & code %notin% c("48074","48096",
                                                    "48069","48065")) |> 
    select(code),
  st_read("geo/communes_2021.shp") |>
    st_transform(st_crs("WGS84")) |>
    filter(code %in%  c("48074", "48096",
                        "48069", "48065")) |>
    st_intersection(coeur) |>
    select(code)
) |> 
  st_union()


# Massif Piémont Cévenol

communes <- fread("French National Parks/Cevennes/communes_piemont.csv",
                  encoding = "UTF-8",
                  colClasses = c("Code" = "character"))


piemont <-  st_read("geo/communes_2021.shp") |> 
  st_transform(st_crs("WGS84")) |> 
  filter(code %in% communes$Code) |> 
  select(code) |> 
  st_union()



ggplot() +
  geom_sf(data = causses,
          fill = "yellow") +
  geom_sf(data = lozere,
          fill = "blue") +
  geom_sf(data = piemont,
          fill = "green") +
  geom_sf(data = vallees,
          fill = "violet") +
  geom_sf(data = aigoual,
          fill = "red")


massifs <- rbind(st_as_sf(aigoual), causses, lozere, piemont, vallees) |> 
  st_as_sf() |> 
  mutate(nom = c("aigoual", "causses", "lozere", "piemont", "vallees"),
         .before = 1)


save(massifs,
     file = "French National Parks/Cevennes/massifs.RData")
