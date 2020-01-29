library(tidyverse)
library(jsonlite)
library(leaflet)

data <- fromJSON("./data/exp-20191127-111721.json") %>% tibble
data <- data[-dim(data)[1],] ## retila el último registro, Danny lo agrega para que formatee bien el json

str(data[[1]]["metadata"])
glimpse(data[[1]]["metadata"][[1]]$gateways)

flat_data <- tibble(
    dev_id  = data[[1]]$dev_id,
    cnt = data[[1]]$counter,
    lat = data[[1]]$payload_fields$gps_1$latitude,
    lng = data[[1]]$payload_fields$gps_1$longitude
   ## ,
   ##  gw_ids = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 2)[[1]],
   ##  gw_time = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 3)[[1]],
   ##  gw_lat = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 9)[[1]],
   ##  gw_lng = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 5)[[1]],
   ##  gw_snr = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 7)[[1]],
   ##  gw_rssi = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 10)[[1]],
   ##  gw_chnl = map_df(data[[1]]["metadata"][[1]]$gateways, `[`, 11)[[1]]
)

## Algunas estructuras son de 10 columnas y otras de 11 (tienen adicional location_source)
map(data[[1]]["metadata"][[1]]$gateways, function(r) {
    as.tibble(r) ## %>% filter(`location_source` == "registry")
})

flat_data$color <- (lapply(flat_data$gw_rssi, function(x)(
    ifelse(x > -60 , "green",
    ifelse(x > -90 && x <= -60 , "gold",
    ifelse( x > -120 && x <= -90, "orange",
    ifelse( x > -150 && x <= -120, "red",
    ifelse( x > -250 && x <= -150, "purple",
           "maroon"))))))) %>% enframe %>% unnest)$value

leaflet(flat_data) %>%
    addTiles() %>%
    addCircleMarkers(~as.numeric(lng), ~as.numeric(lat), color = ~color, popup = ~gw_rssi)

leaflet(flat_data) %>%
    addTiles() %>%
    addCircleMarkers(~as.numeric(lng), ~as.numeric(lat))

flat_data %>% View
