library(tidyverse)
library(RKorAPClient)
library(tidygeocoder)
library(highcharter)
library(sf)
library(geojsonio)
library(jsonlite)

dach_path <- "https://raw.githubusercontent.com/localsofa/DACH/refs/heads/main/DACHtest.geojson"
dach_map <- st_read(dach_path)
dach_geojson <- geojson_json(dach_map, pretty = TRUE)
dach_geo_list <- fromJSON(dach_geojson, simplifyVector = FALSE)


# Suchanfrage
kco <- KorAPConnection(verbose = TRUE)

result <- corpusQuery(kco,
                      query = '"Ha(w|b)ara"',
                      fields = c("corpusTitle", "pubPlaceKey", "pubPlace"),
                      metadataOnly = TRUE) %>%
  fetchAll()

result@collectedMatches

test <- result@collectedMatches
test <- as.data.frame(test)
# view(test)


# Data wrangling
city_data <- test %>%
  mutate(
    city = case_when(
      str_detect(pubPlace, "http") ~ "Wikipedia",
      TRUE ~ str_trim(str_split_fixed(pubPlace, ",|/", 2)[, 1])
    )
  ) %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  summarise(num_sources = n_distinct(corpusTitle)) %>%
  arrange(desc(num_sources))


# Koordinaten
city_coords <- city_data %>%
  geocode(city, method = "osm", lat = latitude, long = longitude)


city_coords_clean <- city_coords %>%
  filter(!is.na(latitude), !is.na(longitude))

city_coords_colored <- test %>%
  mutate(
    city = case_when(
      str_detect(pubPlace, "http") ~ "Wikipedia",
      TRUE ~ str_trim(str_split_fixed(pubPlace, ",|/", 2)[, 1])
    ),
    country = pubPlaceKey
  ) %>%
  filter(!is.na(city), !is.na(country)) %>%
  group_by(city, country) %>%
  summarise(
    num_sources = n_distinct(corpusTitle),
    corpusTitles = paste(unique(corpusTitle), collapse = "<br>"),
    .groups = "drop"
  ) %>%
  geocode(city, method = "osm", lat = latitude, long = longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

# view(city_coords_colored)

# Karte
mapB <- highchart(type = "map") %>%
  hc_add_series(
    mapData = dach_geo_list,
    type = "map",
    name = "DACH Region",
    borderColor = "#5d5654",
    nullColor = "#fffee4",
    showInLegend = FALSE
  ) %>%
  hc_add_series(
    data = filter(city_coords_colored, country == "AT"),
    type = "mapbubble",
    name = "Austria",
    color = "#A2CFF0",
    mapping = hcaes(x = longitude, y = latitude, z = num_sources),
    maxSize = "12%",
    tooltip = list(
      pointFormat = "<b>{point.city}</b><br>Quellen: {point.z}<br><br>Titel:<br>{point.corpusTitles}"
    ),
    cursor = "pointer",
    point = list(
      events = list(
        click = JS("function() { window.open('https://korap.ids-mannheim.de/?q=Hawara+ODER+Habara&ql=cosmas2&cutoff=1', '_blank'); }")
      )
    )
  ) %>%
  hc_add_series(
    data = filter(city_coords_colored, country == "DE"),
    type = "mapbubble",
    name = "Germany",
    color = "#F6A6A6",
    mapping = hcaes(x = longitude, y = latitude, z = num_sources),
    maxSize = "12%",
    tooltip = list(
      pointFormat = "<b>{point.city}</b><br>Quellen: {point.z}<br><br>Titel:<br>{point.corpusTitles}"
    ),
    cursor = "pointer",
    point = list(
      events = list(
        click = JS("function() { window.open('https://korap.ids-mannheim.de/?q=Hawara+ODER+Habara&ql=cosmas2&cutoff=1', '_blank'); }")
      )
    )
  ) %>%
  hc_add_series(
    data = filter(city_coords_colored, country == "CH"),
    type = "mapbubble",
    name = "Switzerland",
    color = "#C3B1E1",
    mapping = hcaes(x = longitude, y = latitude, z = num_sources),
    maxSize = "12%",
    tooltip = list(
      pointFormat = "<b>{point.city}</b><br>Quellen: {point.z}<br><br>Titel:<br>{point.corpusTitles}"
    ),
    cursor = "pointer",
    point = list(
      events = list(
        click = JS("function() { window.open('https://korap.ids-mannheim.de/?q=Hawara+ODER+Habara&ql=cosmas2&cutoff=1', '_blank'); }")
      )
    )
  ) %>%
  hc_legend(enabled = TRUE) %>%
  hc_title(text = "Quellen pro Stadt; Ha[w,b]ara") %>%
  hc_chart(backgroundColor = "#ffffff")

mapB
