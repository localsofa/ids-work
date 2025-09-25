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

# Karte laden
dachTEST <- "https://raw.githubusercontent.com/localsofa/DACH/refs/heads/main/DACHtest.geojson"
dachTESTmap <- st_read(dachTEST)
dachGEO <- geojson_json(dachTESTmap, pretty = TRUE)
dachGeojsonList <- fromJSON(dachGEO, simplifyVector = FALSE)


# Suche
query <- "Hawara ODER Habara"
years <- 1980:2020
vc <- "pubDate in"


df <- KorAPConnection(verbose = TRUE) %>%
  frequencyQuery(
    query,
    paste(vc, years),
    as.alternatives = FALSE,
    ql = "cosmas2"
  )


# Nach Land
query <- "Hawara | Habara" 
countries <- c("AT", "BE", "CH", "DE", "IT", "LU")

vcs <- sprintf("pubPlaceKey=%s", countries)

dfLand <- KorAPConnection(verbose=TRUE) %>%
  frequencyQuery(query, vc=vcs)


AT <- dfLand[dfLand$vc == "pubPlaceKey=AT",]

CH <- dfLand[dfLand$vc == "pubPlaceKey=CH",]

DE <- dfLand[dfLand$vc == "pubPlaceKey=DE",]


# Prozent
percentageDE <- round(DE$totalResults * 100000000 / DE$total, 2)
percentageAT <- round(AT$totalResults * 100000000 / AT$total, 2)
percentageCH <- round(CH$totalResults * 100000000 / CH$total, 2)

ATratio <- AT %>% mutate(ratio = totalResults/total)
DEratio <- DE %>% mutate(ratio = totalResults/total)
CHratio <- CH %>% mutate(ratio = totalResults/total)

TESTratio <- dfLand %>% mutate(percentage = round(totalResults * 10000000 / total, 2))


# Anfang Karte
dachGeojsonList <- jsonlite::fromJSON("DACHtest.geojson", simplifyVector = FALSE)

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
coords

coords <- coords %>%
  mutate(
    successes = c(percentageDE, percentageAT, percentageCH),
    failures = 100 - successes,
    value = successes - failures,
    data = purrr::pmap(list(successes, failures), function(s, f) list(
      list(name = "Treffer", y = s, color = "#A2CFF0"),
      list(name = "Rest", y = f, color = "#F6A6A6")
    ))
  )


pie_json <- jsonlite::toJSON(coords %>% select(id, lat, lon, data), auto_unbox = TRUE)


map <- highchart(type = "map") %>%
  hc_add_series_map(
    map = dachGeojsonList,
    df = coords,
    joinBy = c("id", "id"),
    value = "value",
    name = "Hawara (e^6)",
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

      chart.drawPies();

      Highcharts.addEvent(chart.mapView, 'afterSetView', function () {
        setTimeout(function() {
          chart.drawPies();
        }, 50); 
      });
    }
  ", pie_json)))) %>%
  hc_title(text = "Ha[w,b]ara Verteilung DACH") %>%
  hc_colorAxis(minColor = "#F6A6A6", maxColor = "#A2CFF0") %>%
  hc_subtitle(text = "% in e^6") %>%
  hc_add_onclick_korap_search() # funktioniert nicht?
 
map
