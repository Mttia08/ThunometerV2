
# InfluxDB token for access of data
token <- "tu3zUeCazQobS4TrIIRftQS3Tr4xoZQoZaRf0Ve0iCrU4LZSY1jTS3laCJ_OjwJxWJ6WsKuwXN_tVV10R73hyg=="

# Create InfluxDB client
client <- InfluxDBClient$new(
  url = "https://influx.smcs.abilium.io",
  token = token,
  org = "abilium")

# Build the Flux query
flux_query <- paste0(
  'from(bucket: "smcs") |> ',
  'range(start: -10m) |> ',
  'filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> ',
  'filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> ',
  'filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> ',
  'filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> ',
  'pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")')

# Execute the query
data_current <- client$query(flux_query)

tables <- bind_rows(data_current) |> #binding since better this way, tidy
  mutate(across(starts_with("_"), ~as.POSIXct(., format="%Y-%m-%dT%H:%M:%S%z")))|> #format time
  mutate(Code_grafana = name) #add code grafana for joining

tables|>
  filter(Code_grafana == "166584a41612")


n_distinct(tables$Code_grafana)

meta_1 <- as.tibble(read_delim('Messnetz_Thun_Steffisburg_Uebersicht.csv', delim = ';'))
meta_1 <- subset(meta_1, select = -c(Nr_Strassenlaterne, LCZ))

meta_2 <- as.tibble(read_delim('MetadatenBern.csv', delim = ','))
meta_2 <- rename(meta_2, NORD_CHTOPO = Latitude)
meta_2 <- rename(meta_2, OST_CHTOPO = Longitude)
meta_2 <- subset(meta_2, select = -c(STANDORT_NEU, Doppel_Messnetz_23, New))

meta <- rbind(meta_1, meta_2)

result <- inner_join(tables,meta, by = "Code_grafana",relationship = "many-to-many") |> #many to many since several code grafanas per entry sometimes
  mutate(time = round_date(time, unit = "10 minutes"), 
         Log_NR = as.numeric(Log_NR)) |> # round to 10minutes interval
  group_by(time, Log_NR) |> #group now to mean since some may have several
  summarize(temperature = mean(decoded_payload_temperature, na.rm = TRUE),
            humidity = mean(decoded_payload_humidity, na.rm = TRUE), .groups = "drop") |> #now summarize
  ungroup() |>#important for order
  arrange(Log_NR,time) #now can be arranged
