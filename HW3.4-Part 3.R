## Part 1

library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic"
page <- read_html(url)

# Get the first 6 wikitable tables (2025 to 2020)
tables <- html_elements(page, "table.wikitable")
table_2025 <- html_table(tables[[1]], fill = TRUE)
table_2024 <- html_table(tables[[2]], fill = TRUE)
table_2023 <- html_table(tables[[3]], fill = TRUE)
table_2022 <- html_table(tables[[4]], fill = TRUE)
table_2021 <- html_table(tables[[5]], fill = TRUE)
table_2020 <- html_table(tables[[6]], fill = TRUE)

# Helper function to clean one year's table
clean_airport_table <- function(tbl, year_value) {
  
  names(tbl) <- make.names(names(tbl), unique = TRUE)
  
  airport_col <- grep("^Airport", names(tbl), value = TRUE)[1]
  code_col <- grep("^Code", names(tbl), value = TRUE)[1]
  passengers_col <- grep("Total", names(tbl), value = TRUE)[1]
  
  out <- data.frame(
    year = year_value,
    airport = tbl[[airport_col]],
    code = tbl[[code_col]],
    passengers = tbl[[passengers_col]],
    stringsAsFactors = FALSE
  )
  
  out$airport <- trimws(out$airport)
  out$code <- sub("/.*", "", out$code)
  
  # Extract only the first passenger number
  passenger_text <- regmatches(out$passengers, regexpr("[0-9,]+", out$passengers))
  out$passengers <- as.numeric(gsub(",", "", passenger_text))
  
  out
}

# Clean each year
airports_2025 <- clean_airport_table(table_2025, 2025)
airports_2024 <- clean_airport_table(table_2024, 2024)
airports_2023 <- clean_airport_table(table_2023, 2023)
airports_2022 <- clean_airport_table(table_2022, 2022)
airports_2021 <- clean_airport_table(table_2021, 2021)
airports_2020 <- clean_airport_table(table_2020, 2020)

# Combine all years
airports_all <- rbind(
  airports_2025,
  airports_2024,
  airports_2023,
  airports_2022,
  airports_2021,
  airports_2020
)

# Required airports + three additional airports of my choice
keep_airports <- c(
  "Hartsfield–Jackson Atlanta International Airport",
  "Frankfurt Airport",
  "Beijing Daxing International Airport",
  "Dubai International Airport",
  "Dallas Fort Worth International Airport",
  "Tokyo Haneda Airport"
)

# Keep only the six airports needed
airports_tidy <- airports_all[airports_all$airport %in% keep_airports, ]

# Make airport names a factor so they stay in a useful order
airports_tidy$airport <- factor(airports_tidy$airport, levels = keep_airports)

# Sort the final tidy data
airports_tidy <- airports_tidy[order(airports_tidy$airport, airports_tidy$year), ]

# Final base tidy data
airports_tidy

## Part 2

# Make a copy of the tidy airport data for the table
airports_table <- airports_tidy

# Change passenger numbers to character with commas for readability
airports_table$passengers <- format(airports_table$passengers, big.mark = ",", scientific = FALSE)

# Reshape from long format to wide format
airports_table <- reshape(
  airports_table,
  idvar = c("airport", "code"),
  timevar = "year",
  direction = "wide"
)

# Clean up column names
names(airports_table) <- c("Airport", "Code", "2020", "2021", "2022", "2023", "2024", "2025")

# Sort rows in the same airport order used earlier
airports_table <- airports_table[order(airports_table$Airport), ]

# Show the final table
airports_table

## Part 3

library(ggplot2)
library(scales)

# Make a copy of the tidy airport data for plotting
airports_plot <- airports_tidy

# Create the final plot
ggplot(airports_plot, aes(x = year, y = passengers, color = airport, group = airport)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2020:2025) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Passenger traffic trends for six of the world's busiest airports",
    subtitle = "Comparing annual passenger counts from 2020 to 2025",
    x = "Year",
    y = "Passengers",
    color = "Airport",
    caption = "Source: Wikipedia - List of busiest airports by passenger traffic"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )