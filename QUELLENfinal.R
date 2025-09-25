library(RKorAPClient)
library(highcharter)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(geojson)
library(httr)
library(geojsonio)
library(jsonlite)
library(dplyr)
library(purrr)
library(shiny)
library(DT)

dachTEST <- "https://raw.githubusercontent.com/localsofa/DACH/refs/heads/main/DACHtest.geojson"
dachTESTmap <- st_read(dachTEST)
dachGEO <- geojson_json(dachTESTmap, pretty = TRUE)
dachGeojsonList <- fromJSON(dachGEO, simplifyVector = FALSE)

get_label_coords <- function(geojson, country_code) {
  feature <- Filter(function(f) f$properties$iso_a3 == country_code, geojson$features)[[1]]
  tibble(
    id = country_code,
    lat = feature$properties$label_y,
    lon = feature$properties$label_x
  )
}

coords <- bind_rows(
  get_label_coords(dachGeojsonList, "DEU"),
  get_label_coords(dachGeojsonList, "AUT"),
  get_label_coords(dachGeojsonList, "CHE")
)

kco <- KorAPConnection(verbose = TRUE)
results <- corpusQuery(kco,
                       query = '"Ha(w|b)ara"',
                       fields = c("corpusTitle", "pubPlaceKey"),
                       metadataOnly = TRUE) %>%
  fetchAll()

test <- as.data.frame(results@collectedMatches)

test_filtered <- test %>%
  filter(pubPlaceKey %in% c("AT", "DE", "CH")) %>%
  group_by(pubPlaceKey, corpusTitle) %>%
  summarise(count = n(), .groups = "drop")

test_pct <- test_filtered %>%
  group_by(pubPlaceKey) %>%
  mutate(percent = round(100 * count / sum(count), 2)) %>%
  ungroup()

pie_data_list <- test_pct %>%
  group_by(pubPlaceKey) %>%
  summarise(data = list(
    purrr::map2(corpusTitle, percent, ~ list(name = .x, y = .y))
  )) %>%
  rename(id = pubPlaceKey)

coords_full <- coords %>%
  mutate(id = case_when(
    id == "DEU" ~ "DE",
    id == "AUT" ~ "AT",
    id == "CHE" ~ "CH"
  )) %>%
  left_join(pie_data_list, by = "id") %>%
  mutate(
    data = map(data, function(d) {
      if (is.null(d)) return(list())  
      map(d, ~ list(name = .x$name, y = .x$y))
    }),
    value = 1  
  )

pie_json_sources <- jsonlite::toJSON(
  coords_full %>% select(id, lat, lon, data),
  auto_unbox = TRUE,
  pretty = TRUE
)

mapQ <- highchart(type = "map") %>%
  hc_add_series_map(
    map = dachGeojsonList,
    cursor = "pointer",
    df = coords_full,
    joinBy = c("id", "id"),
    value = "value",
    name = "Source distribution",
    borderColor = "#5d5654"
  ) %>%
  hc_add_dependency("modules/series-on-point.js") %>%
  hc_chart(events = list(load = JS(sprintf("
    function () {
      const chart = this;
      const pieData = %s;
      
      console.log(pieData);

      chart.customPieData = pieData;

      chart.drawPies = function () {
        const existing = chart.series.filter(s => s.type === 'pie');
        existing.forEach(s => s.remove(false));

        chart.customPieData.forEach(function (entry) {
          const point = chart.mapView.projectedUnitsToPixels({
            x: entry.lon,
            y: entry.lat
          });

          console.log('Pie chart position:', point);

          chart.addSeries({
            type: 'pie',
            size: 60,
            center: [point.x, point.y],
            name: entry.id,
            data: entry.data,
            dataLabels: { enabled: false },
            enableMouseTracking: true,
            animation: false,
            zIndex: 6
          }, false);
        });

        chart.redraw(false);
      };

      // Initial draw
      chart.drawPies();

      // Redraw pies on zoom or pan
      Highcharts.addEvent(chart.mapView, 'afterSetView', function () {
        setTimeout(function() {
          chart.drawPies();
        }, 50); 
      });
    }
  ", pie_json_sources)))) %>%
  hc_title(text = "Ha[w,b]ara Quellenverteilung nach Land") %>%
  hc_subtitle(text = "Anteil der Quellen in %") %>%
  hc_colorAxis(minColor = "#F6A6A6", maxColor = "#A2CFF0") %>%
  hc_add_onclick_korap_search()


# make clickable; route to URL of KorAP search --> funktioniert nicht bei allen pie charts
# https://korap.ids-mannheim.de/?q=Hawara+ODER+Habara&ql=cosmas2&cutoff=1
mapQ
