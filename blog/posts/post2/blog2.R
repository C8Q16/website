install.packages("rvest")
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)


url <- "https://opendataphilly.org/datasets/septa-routes-stops-locations/"

page <- read_html(url)

links <- page |> 
  html_elements("a")

links_df <- tibble(
  text = html_text2(links),
  href = html_attr(links, "href")
)

links_df |> 
  filter(str_detect(text, regex("CSV", ignore_case = TRUE)))
highspeed <- read.csv("https://opendata.arcgis.com/api/v3/datasets/af52d74b872045d0abb4a6bbbb249453_0/downloads/data?format=csv&spatialRefId=4326")
trolley <- read.csv("https://opendata.arcgis.com/api/v3/datasets/dd2afb618d804100867dfe0669383159_0/downloads/data?format=csv&spatialRefId=4326")

unique(highspeed$Route)
unique(trolley$LineAbbr)
names(trolley)
head(trolley)

metro <- highspeed |>
  select(
    Line = Route,
    Station,
    Longitude = Longitude,
    Latitude = Latitude
  ) |>
  distinct()

trolley_clean <- trolley |>
  select(
    Line = LineAbbr,
    Station = StopName,
    Longitude = Lon,
    Latitude = Lat
  ) |>
  distinct()

septa_metro <- bind_rows(
  metro,
  trolley_clean
)

head(septa_metro)
unique(septa_metro$Line)

line_summary <- septa_metro |>
  count(Line, name = "Number_of_Stops") |>
  arrange(desc(Number_of_Stops))

line_summary

penn_lat <- 39.9522
penn_lon <- -75.1932

septa_distance <- septa_metro |>
  mutate(
    distance_miles = sqrt(
      ((Latitude - penn_lat) * 69)^2 +
        ((Longitude - penn_lon) * 53)^2
    )
  )

nearest_stops <- septa_distance |>
  arrange(distance_miles) |>
  select(
    Line,
    Station,
    distance_miles
  ) |>
  head(15)

nearest_stops

closest_by_line <- septa_distance |>
  group_by(Line) |>
  slice_min(
    order_by = distance_miles,
    n = 1
  ) |>
  ungroup() |>
  arrange(distance_miles)

closest_by_line

ggplot(
  closest_by_line,
  aes(
    x = reorder(Line, distance_miles),
    y = distance_miles
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Nearest SEPTA Metro/Trolley Stop to Penn",
    x = "SEPTA Line",
    y = "Distance from Penn (miles)"
  ) +
  theme_minimal()

near_penn <- septa_distance |>
  filter(distance_miles <= 1)
unique(near_penn$Line)
near_penn_summary <- near_penn |>
  count(Line, name = "Stops_Within_1_Mile") |>
  arrange(desc(Stops_Within_1_Mile))

near_penn_summary
ggplot(
  near_penn_summary,
  aes(
    x = reorder(Line, Stops_Within_1_Mile),
    y = Stops_Within_1_Mile
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "SEPTA Stops Within One Mile of Penn",
    x = "SEPTA Line",
    y = "Number of Stops"
  ) +
  theme_minimal()
