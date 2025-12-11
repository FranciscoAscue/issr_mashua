pas <- read.table("Data_pasaporte.csv", sep = ";", header = TRUE)

library(tidyverse)
library(sf) 


ga_counties_campgrounds <- ga_counties %>% 
  st_join(ga_campgrounds) %>% 
  group_by(NAMELSAD) %>% 
  summarize(total = sum(!is.na(DESCRIPTOR)))

plot_county <- ggplot() +
  geom_sf(data = ga_counties_campgrounds, aes(fill = total), color = NA) +
  geom_sf(data = ga_state, fill = NA, color = "black", linewidth = 0.25) +
  geom_sf(data = rivers_global_ga, linewidth = 0.3, color = "white") +
  geom_sf(data = rivers_na_ga, linewidth = 0.1, color = "white") +
  geom_sf(data = lakes_global_ga, fill = "white", color = NA) +
  geom_sf(data = lakes_na_ga, fill = "white", color = NA) +
  scale_fill_viridis_c(option = "magma", guide = "none", na.value = "black") +
  coord_sf(crs = ga_crs)
plot_county

(a <- aggregate(aq, de_nuts1, mean, na.rm = TRUE))


library(tidyverse)
a |> filter(time >= "2008-01-01", time < "2008-01-07") |> 
  plot(key.pos = 4)






# ============================
# 0) Paquetes necesarios
# ============================
# install.packages(c("sf", "ggplot2", "dplyr", "readr", "janitor"))

library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)

# ============================
# 1) Leer el CSV de pasaporte
# ============================
# Tu archivo tiene separador ";", así que uso read_csv2
pasaporte_raw <- read_csv2("Data_pasaporte.csv")

# Limpiar nombres de columnas (pasa a snake_case, sin tildes, etc.)
pasaporte <- pasaporte_raw %>%
  janitor::clean_names()

# Echa un ojo a los nombres para confirmar
names(pasaporte)

# Suponiendo que ahora tienes columnas:
# departamento, provincia, distrito, latitud, longitud, etc.

# ============================
# 2) Preparar un resumen por distrito
# ============================
# Por ejemplo: número de accesiones por distrito
pasaporte_dist <- pasaporte %>%
  group_by(departamento, provincia, distrito) %>%
  summarise(n_accesiones = n(), .groups = "drop")

# Normalizar a MAYÚSCULAS para facilitar el join
pasaporte_dist <- pasaporte_dist %>%
  mutate(
    departamento = toupper(trimws(departamento)),
    provincia    = toupper(trimws(provincia)),
    distrito     = toupper(trimws(distrito))
  )

# ============================
# 3) Leer el GeoJSON distrital de Perú con sf
# ============================
url_geojson <- "https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_distrital_simple.geojson"

peru_distritos <- st_read(url_geojson, quiet = TRUE)

# Mira los nombres de columnas para identificar cómo se llama
# el campo de distrito, provincia, departamento, etc.
names(peru_distritos)
head(peru_distritos)

# Supongamos que en el geojson las columnas relevantes se llaman, por ejemplo:
#  - "NOMBDEP"  : nombre departamento
#  - "NOMBPROV" : nombre provincia
#  - "NOMBDIST" : nombre distrito
#
# Ajusta estos nombres a lo que realmente veas con names(peru_distritos).

# Normalizamos también a mayúsculas
peru_distritos <- peru_distritos %>%
  mutate(
    NOMBDEP  = toupper(trimws(NOMBDEP)),
    NOMBPROV = toupper(trimws(NOMBPROV)),
    NOMBDIST = toupper(trimws(NOMBDIST))
  )

# ============================
# 4) Unir el resumen de accesiones al shapefile
# ============================
# Unimos por departamento + provincia + distrito (más robusto que solo distrito)
peru_mapa <- peru_distritos %>%
  left_join(
    pasaporte_dist,
    by = c(
      "NOMBDEP"  = "departamento",
      "NOMBPROV" = "provincia",
      "NOMBDIST" = "distrito"
    )
  )

# ============================
# 5) Mapa coroplético (relleno según n_accesiones)
# ============================
ggplot(peru_mapa) +
  geom_sf(aes(fill = n_accesiones), color = "grey30", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey95",
    name = "N° accesiones"
  ) +
  labs(
    title = "Distribución de accesiones por distrito",
    subtitle = "Datos de pasaporte sobre el mapa distrital del Perú",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    legend.position = "right"
  )

library(ggplot2)
library(dplyr)
library(sf)

# Asegúrate que tienes puntos con latitud/longitud en columnas numéricas
# Si ya tienes pasaporte_pts_sf, sacamos los coords como data.frame:
pasaporte_pts_df <- pasaporte_pts_sf %>%
  st_drop_geometry()

ggplot(peru_mapa) +
  geom_sf(aes(fill = n_accesiones), color = "grey30", size = 0.1) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey95",
    name = "N° accesiones"
  ) +
  labs(
    title = "Número de accesiones por distrito",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  )



# 1) Creamos una grilla hexagonal que cubra la extensión de los puntos
# Ajusta cellsize según la escala que quieras (en grados)
bbox_pts <- st_bbox(pasaporte_pts_sf)

grid_hex <- st_make_grid(
  st_as_sfc(bbox_pts),
  cellsize = 0.3,        # prueba valores entre 0.1 y 1 según lo denso que esté
  square = FALSE
)

grid_hex_sf <- st_sf(id_hex = 1:length(grid_hex), geometry = grid_hex)

# 2) Hacemos join espacial puntos -> hexágonos
pts_en_hex <- st_join(
  pasaporte_pts_sf,
  grid_hex_sf,
  join = st_within
)

# 3) Contamos puntos por hexágono
hex_counts <- pts_en_hex %>%
  st_drop_geometry() %>%
  group_by(id_hex) %>%
  summarise(n_accesiones = n(), .groups = "drop") %>%
  right_join(grid_hex_sf, by = "id_hex") %>%
  st_as_sf()

# 4) Mapa
ggplot() +
  geom_sf(data = peru_distritos, fill = "grey98", color = "grey85", size = 0.1) +
  geom_sf(data = hex_counts, aes(fill = n_accesiones), color = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    na.value = "transparent",
    name = "N° accesiones"
  ) +
  labs(
    title = "Mapa hexagonal de accesiones",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank()
  )





# 1) Definir la extensión (puedes usar el bbox de los puntos o de Perú)
bbox <- st_bbox(pasaporte_pts_sf)  # o st_bbox(peru_distritos)

# 2) Grilla hexagonal sobre esa extensión
hex_grid <- st_make_grid(
  st_as_sfc(bbox),
  cellsize = 0.3,   # ajusta la resolución
  square   = FALSE
)

hex_sf <- st_sf(id_hex = seq_along(hex_grid), geometry = hex_grid)

# 3) Join espacial: asignar cada punto a un hexágono
pts_con_hex <- st_join(pasaporte_pts_sf, hex_sf, join = st_within)

# 4) Contar puntos por hexágono
hex_counts <- pts_con_hex |>
  st_drop_geometry() |>
  group_by(id_hex) |>
  summarise(n_accesiones = n(), .groups = "drop") |>
  right_join(hex_sf, by = "id_hex") |>
  st_as_sf()

# 5) Mapa hexagonal (heatmap discreto)
ggplot() +
  geom_sf(data = peru_distritos, fill = "grey98", color = "grey85", size = 0.1) +
  geom_sf(data = hex_counts, aes(fill = n_accesiones), color = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    na.value = "transparent",
    name = "N° accesiones"
  ) +
  labs(
    title = "Mapa hexagonal de densidad de accesiones",
    x = NULL, y = NULL
  ) +
  theme_minimal()




#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


# ============================
# 0) Paquetes
# ============================
# install.packages(c("sf", "dplyr", "readr", "janitor", "ggplot2"))

library(sf)
library(dplyr)
library(readr)
library(janitor)
library(ggplot2)

# ============================
# 1) Leer CSV de pasaporte
# ============================
pasaporte_raw <- read_csv2("Data_pasaporte.csv")   # separador ";"
pasaporte <- pasaporte_raw %>% clean_names()

# Revisa los nombres para ver cómo se llaman tus columnas:
names(pasaporte)

# Asumo que tienes columnas: departamento, provincia, latitud, longitud
# Si los nombres son distintos, cámbialos aquí o crea alias:
# pasaporte <- pasaporte %>%
#   rename(
#     departamento = DEPARTAMENTO,
#     provincia    = PROVINCIA,
#     latitud      = LATITUD,
#     longitud     = LONGITUD
#   )

# Crear objeto sf de puntos (CRS WGS84)
pasaporte_pts_sf <- st_as_sf(
  pasaporte,
  coords = c("longitud", "latitud"),  # OJO: ajusta si tus nombres son otros
  crs = 4326
)


# ============================
# 2) Leer polígonos departamentales y provinciales
# ============================

url_dep <- "https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_departamental_simple.geojson"
url_prov <- "https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_provincial_simple.geojson"

peru_dep <- st_read(url_dep, quiet = TRUE)
peru_prov <- st_read(url_prov, quiet = TRUE)

names(peru_dep)
names(peru_prov)

# Normalizar nombres a mayúsculas para hacer joins robustos
peru_dep <- peru_dep %>%
  mutate(
    NOMBDEP = toupper(trimws(NOMBDEP))
  )

peru_prov <- peru_prov %>%
  mutate(
    NOMBPROV = toupper(trimws(NOMBPROV)), # nombre provincia
    FIRST_NOMB = toupper(trimws(FIRST_NOMB)) # nombre departamento
  )




# ============================
# 3) Conteo de accesiones por DEPARTAMENTO
# ============================

pasaporte_dep <- pasaporte %>%
  mutate(
    departamento = toupper(trimws(departamento))
  ) %>%
  group_by(departamento) %>%
  summarise(
    n_accesiones = n(),
    .groups = "drop"
  )

# Join con el polígono departamental
peru_dep_mapa <- peru_dep %>%
  left_join(
    pasaporte_dep,
    by = c("NOMBDEP" = "departamento")
  )

# ============================
# 4) Mapa departamental (choropleth)
# ============================
ggplot(peru_dep_mapa) +
  geom_sf(aes(fill = n_accesiones), color = "grey30", size = 0.2) +
  scale_fill_viridis_c(
    option   = "magma",
    na.value = "grey95",
    name     = "N° accesiones"
  ) +
  labs(
    title = "Accesiones por departamento",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position  = "right"
  )







# ============================
# 5) Conteo de accesiones por PROVINCIA
# ============================

pasaporte_prov <- pasaporte %>%
  mutate(
    departamento = toupper(trimws(departamento)),
    provincia    = toupper(trimws(provincia))
  ) %>%
  group_by(departamento, provincia) %>%
  summarise(
    n_accesiones = n(),
    .groups = "drop"
  )

# En el geojson:
#   FIRST_NOMB = nombre del departamento
#   NOMBPROV   = nombre de la provincia
peru_prov_mapa <- peru_prov %>%
  left_join(
    pasaporte_prov,
    by = c(
      "FIRST_NOMB" = "departamento",
      "NOMBPROV"   = "provincia"
    )
  )

# ============================
# 6) Mapa provincial (choropleth)
# ============================
ggplot(peru_prov_mapa) +
  geom_sf(aes(fill = n_accesiones), color = "grey30", size = 0.1) +
  scale_fill_viridis_c(
    option   = "plasma",
    na.value = "grey95",
    name     = "N° accesiones"
  ) +
  labs(
    title = "Accesiones por provincia",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position  = "right"
  )







ggplot() +
  geom_sf(data = peru_prov_mapa, aes(fill = n_accesiones), color = "grey40", size = 0.1) +
  geom_sf(data = pasaporte_pts_sf, size = .5, alpha = 0.7, color = "white") +
  scale_fill_viridis_c(
    option   = "plasma",
    na.value = "grey95",
    name     = "N° accesiones"
  ) +
  labs(
    title = "Accesiones por provincia (polígonos + puntos)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank()
  )




# coords desde geometry una sola vez
coords <- st_coordinates(pasaporte_pts_sf)
pasaporte_df <- cbind(pasaporte |> mutate(
  departamento = toupper(trimws(departamento)),
  provincia    = toupper(trimws(provincia))
), as.data.frame(coords))

# Heatmap faceteado por departamento
ggplot() +
  stat_density_2d_filled(
    data = pasaporte_df,
    aes(x = X, y = Y, fill = after_stat(level)),
    alpha = 0.8,
    contour_var = "ndensity"
  ) +
  facet_wrap(~ departamento) +
  coord_equal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(
    title = "Densidad de accesiones por departamento",
    x = NULL, y = NULL
  ) +
  theme_minimal()







# Paquetes
# install.packages(c("readr", "dplyr", "janitor", "lubridate", "ggplot2"))

library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)

# Leer CSV (separador ;)
pasaporte_raw <- read_csv2("Data_pasaporte.csv")
pasaporte <- pasaporte_raw %>% clean_names()

# Revisa nombres para ubicar provincia y fecha
names(pasaporte)

# Parsear fecha y extraer año
pasaporte <- pasaporte %>%
  mutate(
    # ajusta 'fecha_colecta' si tu columna tiene otro nombre
    fecha_de_colecta = ymd(fecha_de_colecta),     # si es dd/mm/yyyy
    # fecha_colecta = ymd(fecha_colecta),   # usa esto si fuera yyyy-mm-dd
    anio = year(fecha_de_colecta),
    provincia = toupper(trimws(provincia))  # normalizar nombres
  )


prov_anio <- pasaporte %>%
  filter(!is.na(fecha_de_colecta), !is.na(provincia)) %>%
  count(provincia, anio, departamento)   # n = número de accesiones



ggplot(prov_anio, aes(x = anio, y = n)) +
  geom_col() +
  facet_wrap(~ provincia) +
  labs(
    title = "Número de accesiones por año y provincia de colecta",
    x = "Año de colecta",
    y = "N° de accesiones"
  ) +
  theme_minimal()

ggplot(prov_anio, aes(x = anio, y = provincia, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(name = "N° accesiones") +
  labs(
    title = "Heatmap de accesiones por provincia y año",
    x = "Año de colecta",
    y = "Provincia"
  ) +
  theme_minimal()



prov_totales <- prov_anio %>%
  group_by(provincia) %>%
  summarise(total = sum(n), .groups = "drop")

prov_con_mas <- prov_totales %>%
  filter(total >= 5)   # por ejemplo, provincias con ≥ 5 accesiones

prov_anio_filtrado <- prov_anio %>%
  semi_join(prov_con_mas, by = "provincia")

ggplot(prov_anio_filtrado, aes(x = anio, y = n, fill = departamento )) +
  geom_col() +
  facet_wrap(~ provincia) +
  labs(
    title = "Accesiones por año y provincia (solo provincias con ≥ 5 accesiones)",
    x = "Año de colecta",
    y = "N° de accesiones"
  ) +
  theme_minimal()

library(BiocManager)
# BiocManager::install('BiocHubsShiny')
library(AnnotationHub)
library(AnnotationHub)
library(AHMeSHDbs)
ah <- AnnotationHub()
BiocHubsShiny::BiocHubsShiny()

library(GenomicFeatures)
leishmania <- ah[['AH74257']]

txdb <- makeTxDbFromGRanges(leishmania)
columns(txdb)
keytypes(txdb)

ids <- head(keys(txdb, keytype = "GENEID"), 1000)
ids
