files <- list.files(path="/Users/giub/Desktop/Biel_01_csv", pattern=".csv", full.names=TRUE)

data_biel <- data.frame()

for (file in files) {
  temp_data <- read.csv(file, skip = 1)
  
  temp_data <- temp_data[, 3]
  
  temp_data <- data.frame(temperature = temp_data)
  
  temp_data$Log_NR <- as.numeric(substr(file, nchar(file) - 6, nchar(file) - 4))

  data_biel <- rbind(data_biel, temp_data)
}

data_biel <- data_biel |>
  group_by(Log_NR) |>
  summarise(temperature = mean(as.numeric(temperature), na.rm = TRUE))

