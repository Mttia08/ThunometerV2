combined <- as.tibble(read_delim('combined.csv', delim = ','))

combined <- combined[colSums(!is.na(combined)) > 0]
combined <- combined|>
  select(-c('STANDORT', 'Code_grafana', 'NORD_CHTOPO', 'OST_CHTOPO', 'LV_03_N', 'LV_03_E', 'Art', 'ZUSTAENDIG', 'BEFESTIGUNG', 'Nr_Strassenlaterne', 'LCZ', 'Start', 'Quali', 'FLAC_25', 'TPI_25', 'ROU_25', 'TPI_150',
            'year', 'day', 'month', 'hour','Log_Nr', 'TPI_1000', "TPI_150", 'timestamp')) |>
  mutate(temperature = temperature - temp) |>
  drop_na()