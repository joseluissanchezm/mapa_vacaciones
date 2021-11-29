library(dplyr)
library(ggplot2)
library(jsonlite) # Lectura de datos de Google
library(osmdata) # Obtención de mapa
# Historico de movimientos de google ---------
data <- fromJSON("data/Historial de ubicaciones.json")
data <- data$locations
# Timestamp a tipo Date
data <- data %>%
  mutate(
    timestampMs = as.numeric(timestampMs)/1000
  ) %>%
  mutate(
    date = as.POSIXct(timestampMs, origin="1970-01-01")
  )

data <- data %>%
  relocate(date, .before = timestampMs)

# Filtrar los datos para los días de vacaciones

data <- data %>%
  filter(
    date >= "2021-11-19 21:45:00"  & date <= "2021-11-23 11:30:00"
  )

# Transformamos el tipo de longitud y latitud

data <- data %>%
  mutate(
    lat = latitudeE7/1e7,
    lon = longitudeE7/1e7
  ) %>%
  select(-latitudeE7, -longitudeE7)

# Seleccionamos las columnas necesarias para el mapa

data <- data %>%
  select(date, lat, lon)

# Mapa de Granada --------

# Obtenemos el contorno de la ciudad
bb <- getbb("Granada")
# Obtenemos las calles
streets <- bb%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary",
                            "secondary", "tertiary")) %>%
  osmdata_sf()

# Y las calles pequeñas
small_streets <- bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()



# Plot ---------
# Importamos la fuente que más nos guste
library(showtext)
font.add(family = "horrendo",
         regular = "data/horrendo.ttf")
showtext_auto()

# ggplot
ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .8,
          alpha = .9) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .2) +
  geom_point(
    data = data,
    aes(
      x = lon,
      y = lat
    ), size = .4, col = "red", alpha = 0.6

  ) +
  coord_sf(xlim = c(-3.65, -3.55),
           ylim = c(37.14, 37.21),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828"),
    text = element_text(family="horrendo"),
    plot.title = element_text(colour = "white", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(colour = "white", size = 16, face = "bold", hjust = 0.5)
  ) +
  ggtitle(
    "GRANADA",
    subtitle = "Noviembre - 2021"
  )
