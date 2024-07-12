source("load_packages.R")
packages <- c("jsonlite", "leaflet", "osmdata", "sf", 'tidyverse', 'influxdbclient', 'doParallel', 'Metrics', 'shiny')
load_packages(packages)

source("get_data.R")

# Extract last temperature value from results
latest_temperatures <- result %>%
  group_by(Log_NR) %>%
  summarise(Last_Temperature = round(last(temperature), digits = 2))

# Show measurements on a map
offline_stations <- c()

meta_1 <- as.tibble(read_delim('Messnetz_Thun_Steffisburg_Uebersicht.csv', delim = ';'))
meta_1 <- subset(meta_1, select = -c(Nr_Strassenlaterne, LCZ))
meta_1$Log_NR <- as.numeric(meta_1$Log_NR)

meta_2 <- as.tibble(read_delim('MetadatenBern.csv', delim = ','))
meta_2 <- rename(meta_2, NORD_CHTOPO = Latitude)
meta_2 <- rename(meta_2, OST_CHTOPO = Longitude)
meta_2 <- subset(meta_2, select = -c(STANDORT_NEU, Doppel_Messnetz_23, New))
meta_2$Log_NR <- as.numeric(meta_2$Log_NR)

meta <- bind_rows(meta_1, meta_2)
meta <- meta |>
  select(c("Log_NR", "NORD_CHTOPO", "OST_CHTOPO"))

for (log_nr in meta$Log_NR)
  if (!(log_nr %in% result$Log_NR) && !(log_nr %in% offline_stations))
    offline_stations <- c(offline_stations, log_nr)

offline_meta <- meta |>
  filter(Log_NR %in% offline_stations)
online_meta <- meta |>
  filter(!(Log_NR %in% offline_stations))

online_meta <- merge(online_meta, latest_temperatures, by = "Log_NR")

bbox <- list(min_lon = 7.358, min_lat = 46.716667, max_lon = 7.65, max_lat = 47.02)

pal <- colorNumeric(palette = c("blue", "lightblue", "white", "orange", "red", "darkred"), domain = online_meta$Last_Temperature)

time <- sort(as.character(result$time))[1]
time_split <- unlist(strsplit(time, " "))

ui <- fluidPage(
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map"),  
  paste("Daten vom", time_split[1], "um", time_split[2])
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      fitBounds(lng1 = bbox$min_lon, lat1 = bbox$min_lat, lng2 = bbox$max_lon, lat2 = bbox$max_lat) |>
      addCircleMarkers(
        data = online_meta,
        ~OST_CHTOPO, ~NORD_CHTOPO,
        label = ~paste(Last_Temperature, "°C"),
        radius = 6,
        color = "black",
        fillColor = ~pal(Last_Temperature),
        fill = TRUE,
        stroke = TRUE,
        fillOpacity = 1,
        weight = 2) |>
      addCircleMarkers(
        data = offline_meta,
        ~OST_CHTOPO, ~NORD_CHTOPO,
        label = ~Log_NR,
        radius = 4,
        color = "black",
        fill = TRUE,
        stroke = TRUE,
        fillOpacity = 1,
        weight = 2) |>
      addLegend(
        pal = pal,
        values = online_meta$Last_Temperature, 
        opacity = 0.7, 
        title = "Temperatur (°C)",
        position = "topright")
  })
}

shinyApp(ui, server)
