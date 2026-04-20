library(tidyverse)
library(sf)
library(leaflet)

traffic_sf <- st_read("~/Downloads/Annual_Average_Daily_Traffic.geojson") %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    BACK_AADT = as.numeric(BACK_AADT)
  )

venues <- data.frame(
  name = c("LA Memorial Coliseum", "2028 Stadium (SoFi)", "DTLA Arena (Crypto.com)",
           "Intuit Dome", "Rose Bowl Stadium", "Dodger Stadium",
           "Exposition Park Stadium (BMO)", "Venice Beach", "Carson Stadium (Dignity Health)"),
  lat = c(34.0141, 33.9535, 34.0430,
          33.9583, 34.1613, 34.0739,
          34.0128, 33.9850, 33.8644),
  lon = c(-118.2879, -118.3390, -118.2673,
          -118.3416, -118.1676, -118.2400,
          -118.2841, -118.4695, -118.2615)
)

traffic_sf %>%
  filter(!is.na(BACK_AADT)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = ~pmin(sqrt(BACK_AADT / 50000) * 2, 8),
    color = ~colorNumeric("YlOrRd", BACK_AADT)(BACK_AADT),
    stroke = FALSE, fillOpacity = 0.7,
    popup = ~paste0("<b>", DESCRIPTION, "</b><br>",
                    "Route: ", RTE, "<br>",
                    "County: ", CNTY, "<br>",
                    "AADT: ", formatC(BACK_AADT, format = "d", big.mark = ","))
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlOrRd", -traffic_sf$BACK_AADT, na.color = NA, reverse = TRUE),
    values = ~-BACK_AADT,
    labFormat = labelFormat(transform = function(x) -x),
    title = "Daily Traffic (AADT)",
    opacity = 0.7 
  )%>%
  addMarkers(
    data = venues,
    lng = ~lon, lat = ~lat,
    popup = ~name
  )
