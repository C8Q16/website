# ============================================================
# Blog 7: Exploring U.S. State Consumption, Income, and Prices
# Data source: U.S. Bureau of Economic Analysis (BEA)
# Analysis period: 2008–2024
# Geography: 50 U.S. states, excluding Washington, D.C.
# ============================================================


# ------------------------------------------------------------
# 1. Install and load packages
# ------------------------------------------------------------

packages <- c(
  "readr", "dplyr", "tidyr",
  "stringr", "plotly", "htmlwidgets"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(htmlwidgets)

options(timeout = 300)

dir.create("data_raw", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

start_year <- 2008
end_year <- 2024

# Change this later to explore another year.
selected_year <- 2024


# ------------------------------------------------------------
# 2. zip file load
# ------------------------------------------------------------

pce_zip <- "/Users/yangyixin/Desktop/研一上/AEDS6400/blog7/SAPCE.zip"       # SAPCE.zip
income_zip <- "/Users/yangyixin/Desktop/研一上/AEDS6400/blog7/SAINC.zip"    # SAINC.zip
price_zip <- "/Users/yangyixin/Desktop/研一上/AEDS6400/blog7/SARPP.zip"     # SARPP.zip




# ------------------------------------------------------------
# 3. read csv form zip file
# ------------------------------------------------------------
read_bea_zip <- function(zip_path, pattern) {
  

  if (!file.exists(zip_path)) {
    stop("no file：", zip_path)
  }
  

  files <- unzip(zip_path, list = TRUE)$Name
  target <- files[grepl(pattern, basename(files))]
  
  if (length(target) != 1) {
    stop("number of csv files is not 1")
  }
  
  # read csv
  connection <- unz(zip_path, target, open = "r")
  on.exit(close(connection))
  
  data <- read.csv(
    connection,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  

  names(data) <- sub("^\ufeff", "", names(data))
  
  data
}

pce_raw <- read_bea_zip(
  pce_zip,
  "^SAPCE2__ALL_AREAS_.*\\.csv$"
)

income_raw <- read_bea_zip(
  income_zip,
  "^SAINC1__ALL_AREAS_.*\\.csv$"
)

price_raw <- read_bea_zip(
  price_zip,
  "^SARPP_STATE_.*\\.csv$"
)


dim(pce_raw)
dim(income_raw)
dim(price_raw)

pce_raw[1:5, 1:10]

# ------------------------------------------------------------
# 4. Convert BEA wide tables into tidy long tables
# ------------------------------------------------------------

# Original format:
# state | indicator | 2008 | 2009 | 2010 | ...
#
# Tidy format:
# state | indicator | year | value
## This is the foundation for pivot tables and dashboards.

income_raw$GeoName <- trimws(
  gsub("*", "", income_raw$GeoName, fixed = TRUE)
)

pce_raw$GeoName <- trimws(
  gsub("*", "", pce_raw$GeoName, fixed = TRUE)
)

price_raw$GeoName <- trimws(
  gsub("*", "", price_raw$GeoName, fixed = TRUE)
)


tidy_bea <- function(data) {
  
  data %>%
    filter(GeoName %in% state.name) %>%
    
    mutate(
      state = GeoName,
      
      # BEA codes can contain spaces and quotation marks.
      state_fips = str_pad(
        str_remove_all(GeoFIPS, '[^0-9]'),
        width = 5,
        side = "left",
        pad = "0"
      ),
      
      line_code = as.integer(str_trim(LineCode)),
      description = str_squish(Description)
    ) %>%
    
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = "raw_value"
    ) %>%
    
    mutate(
      year = as.integer(year),
      
      # Suppression symbols or unavailable observations become NA.
      value = suppressWarnings(
        parse_number(raw_value)
      )
    ) %>%
    
    filter(
      year >= start_year,
      year <= end_year
    ) %>%
    
    select(
      state_fips, state, year,
      line_code, description,
      unit = Unit, value
    )
}

pce_long <- tidy_bea(pce_raw)
income_long <- tidy_bea(income_raw)
price_long <- tidy_bea(price_raw)


# ------------------------------------------------------------
# 5. Select consumption categories
# ------------------------------------------------------------

# These codes belong specifically to BEA table SAPCE2.
# Total is retained as a reference, not added to its components.
#
# The selected detailed categories are not an exhaustive
# decomposition of total PCE.

category_lookup <- tibble(
  line_code = c(
    1L, 4L, 9L, 10L, 11L,
    15L, 16L, 17L, 18L, 19L, 20L
  ),
  
  category = c(
    "Total consumption",
    "Motor vehicles and parts",
    "Groceries and beverages",
    "Clothing and footwear",
    "Gasoline and energy goods",
    "Housing and utilities",
    "Health care",
    "Transportation services",
    "Recreation services",
    "Food services and accommodations",
    "Financial services and insurance"
  )
)

consumption <- pce_long %>%
  inner_join(category_lookup, by = "line_code") %>%
  select(
    state_fips, state, year,
    category, pce_per_capita = value
  )

total_consumption <- consumption %>%
  filter(category == "Total consumption") %>%
  select(
    state_fips, year,
    total_pce_per_capita = pce_per_capita
  )

consumption <- consumption %>%
  left_join(
    total_consumption,
    by = c("state_fips", "year")
  ) %>%
  mutate(
    share_pct = 100 *
      pce_per_capita / total_pce_per_capita
  )


# ------------------------------------------------------------
# 6. Extract income, population, and overall price levels
# ------------------------------------------------------------

# SAINC1:
# Line 2 = Population
# Line 3 = Per capita personal income

income <- income_long %>%
  filter(line_code == 3L) %>%
  select(
    state_fips, year,
    income_per_capita = value
  )

population <- income_long %>%
  filter(line_code == 2L) %>%
  select(
    state_fips, year,
    population = value
  )

# SARPP:
# Line 1 = RPP, all items
# U.S. price level in each year = 100

prices <- price_long %>%
  filter(line_code == 1L) %>%
  select(
    state_fips, year,
    rpp = value
  )


# ------------------------------------------------------------
# 7. Build a state-year analytical table
# ------------------------------------------------------------

state_year <- consumption %>%
  filter(category == "Total consumption") %>%
  select(
    state_fips, state, year,
    total_pce_per_capita = pce_per_capita
  ) %>%
  
  left_join(
    income,
    by = c("state_fips", "year")
  ) %>%
  
  left_join(
    population,
    by = c("state_fips", "year")
  ) %>%
  
  left_join(
    prices,
    by = c("state_fips", "year")
  ) %>%
  
  mutate(
    region = state.region[match(state, state.name)],
    state_abbr = state.abb[match(state, state.name)],
    
    # Adjust for differences in regional price levels.
    # This is NOT a constant-dollar time series.
    price_adjusted_income =
      income_per_capita / (rpp / 100)
  ) %>%
  
  group_by(state) %>%
  arrange(year, .by_group = TRUE) %>%
  
  mutate(
    pce_growth_pct = 100 *
      (total_pce_per_capita /
         lag(total_pce_per_capita) - 1),
    
    income_growth_pct = 100 *
      (income_per_capita /
         lag(income_per_capita) - 1)
  ) %>%
  
  ungroup() %>%
  group_by(year) %>%
  
  mutate(
    nominal_income_rank =
      min_rank(desc(income_per_capita)),
    
    adjusted_income_rank =
      min_rank(desc(price_adjusted_income)),
    
    # Positive = improved rank after price adjustment.
    rank_improvement =
      nominal_income_rank - adjusted_income_rank
  ) %>%
  
  ungroup()


# ------------------------------------------------------------
# 8. Validate the merged data
# ------------------------------------------------------------

expected_rows <- 50 * (end_year - start_year + 1)

stopifnot(
  nrow(state_year) == expected_rows,
  
  nrow(distinct(state_year, state_fips, year)) ==
    expected_rows,
  
  !anyNA(state_year$income_per_capita),
  !anyNA(state_year$total_pce_per_capita),
  !anyNA(state_year$population),
  !anyNA(state_year$rpp),
  
  all(state_year$rpp > 0),
  
  nrow(consumption) ==
    expected_rows * nrow(category_lookup),
  
  !anyNA(consumption$pce_per_capita)
)


missing_states <- setdiff(
  state.name,
  unique(income_long$state)
)

print(missing_states)


print(
  unique(income_raw$GeoName)
)








cat("\nValidation passed.\n")
cat("Number of states:", n_distinct(state_year$state), "\n")
cat("Years:", min(state_year$year), "-", max(state_year$year), "\n")
cat("State-year observations:", nrow(state_year), "\n")


# ------------------------------------------------------------
# 9. Create pivot tables
# ------------------------------------------------------------

# Pivot A:
# Rows = states
# Columns = consumption categories
# Values = annual dollars per person

pivot_spending <- consumption %>%
  filter(year == selected_year) %>%
  select(state, category, pce_per_capita) %>%
  pivot_wider(
    names_from = category,
    values_from = pce_per_capita
  ) %>%
  arrange(desc(`Total consumption`))

# Pivot B:
# Rows = states
# Columns = consumption categories
# Values = share of total PCE (%)

pivot_shares <- consumption %>%
  filter(
    year == selected_year,
    category != "Total consumption"
  ) %>%
  select(state, category, share_pct) %>%
  pivot_wider(
    names_from = category,
    values_from = share_pct
  )

# Pivot C:
# Rows = regions
# Columns = years
# Values = population-weighted spending per person

pivot_region <- state_year %>%
  group_by(region, year) %>%
  summarise(
    pce_per_capita = weighted.mean(
      total_pce_per_capita,
      w = population
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = pce_per_capita
  )

print(pivot_spending, n = 10, width = Inf)
print(pivot_region, width = Inf)

# Optional: open these in RStudio's data viewer.
View(pivot_spending)
View(pivot_shares)


# ------------------------------------------------------------
# 10. Figure 1: Interactive category rankings
# ------------------------------------------------------------

# Dropdown changes the consumption category.
# Each category gets its own correctly sorted state ranking.

categories <- category_lookup$category


chart_sets <- lapply(categories, function(current_category) {
  
  consumption %>%
    filter(
      year == selected_year,
      category == current_category
    ) %>%
    arrange(pce_per_capita) %>%
    mutate(
      hover_text = paste0(
        state,
        "<br>Category: ", category,
        "<br>Year: ", year,
        "<br>Spending per person: $",
        format(
          round(pce_per_capita),
          big.mark = ",",
          trim = TRUE
        ),
        "<br>Share of total: ",
        round(share_pct, 1), "%"
      )
    )
})


stopifnot(
  all(vapply(chart_sets, nrow, integer(1)) == 50L)
)


initial_data <- chart_sets[[1]]


fig1 <- plot_ly(
  x = initial_data$pce_per_capita,
  y = initial_data$state,
  text = initial_data$hover_text,
  
  type = "bar",
  orientation = "h",
  height = 1100,
  
  marker = list(color = "#2878A5"),
  hovertemplate = "%{text}<extra></extra>"
)


buttons <- lapply(seq_along(categories), function(i) {
  
  current_data <- chart_sets[[i]]
  
  list(
    method = "update",
    label = categories[i],
    
    args = list(
      
      
      list(
        x = list(current_data$pce_per_capita),
        y = list(current_data$state),
        text = list(current_data$hover_text)
      ),
      
      # 
      list(
        title = list(
          text = paste0(
            categories[i], " by State, ", selected_year
          )
        ),
        yaxis = list(
          title = list(text = ""),
          categoryorder = "array",
          categoryarray = current_data$state,
          automargin = TRUE
        ),
        xaxis = list(autorange = TRUE)
      )
    )
  )
})

fig1 <- fig1 %>%
  layout(
    title = list(
      text = paste0(
        "Total Consumption by State, ", selected_year
      )
    ),
    
    margin = list(l = 150, r = 30, t = 160, b = 70),
    
    xaxis = list(
      title = list(
        text = "Annual spending per person (current USD)"
      ),
      tickprefix = "$",
      tickformat = ",.0f"
    ),
    
    yaxis = list(
      title = list(text = ""),
      categoryorder = "array",
      categoryarray = initial_data$state,
      automargin = TRUE
    ),
    
    showlegend = FALSE,
    
    updatemenus = list(
      list(
        type = "dropdown",
        x = 0,
        y = 1.08,
        xanchor = "left",
        yanchor = "bottom",
        buttons = buttons
      )
    )
  )


fig1


htmlwidgets::saveWidget(
  fig1,
  "output/figure1_rankings.html",
  selfcontained = FALSE
)

# ------------------------------------------------------------
# 11. Figure 2: Income and prices, with a year slider
# ------------------------------------------------------------

fig2 <- plot_ly(
  data = state_year,
  
  x = ~income_per_capita,
  y = ~rpp,
  
  color = ~region,
  colors = "Set2",
  
  frame = ~year,
  
  type = "scatter",
  mode = "markers",
  
  marker = list(
    size = 11,
    opacity = 0.8,
    line = list(color = "white", width = 0.5)
  ),
  
  text = ~paste0(
    state,
    "<br>Year: ", year,
    "<br>Income per person: $",
    format(round(income_per_capita), big.mark = ",", trim = TRUE),
    "<br>Price level (U.S. = 100): ",
    round(rpp, 1),
    "<br>Consumption per person: $",
    format(round(total_pce_per_capita), big.mark = ",", trim = TRUE)
  ),
  
  hovertemplate = "%{text}<extra></extra>"
) %>%
  
  layout(
    title = "Do Higher-Income States Have Higher Prices?",
    
    xaxis = list(
      title = "Personal income per person (current USD)",
      tickprefix = "$",
      range = c(
        0,
        max(state_year$income_per_capita) * 1.08
      )
    ),
    
    yaxis = list(
      title = "Regional price parity (U.S. = 100)",
      range = c(
        min(state_year$rpp) - 3,
        max(state_year$rpp) + 3
      )
    ),
    
    shapes = list(
      list(
        type = "line",
        xref = "paper",
        x0 = 0,
        x1 = 1,
        y0 = 100,
        y1 = 100,
        line = list(
          dash = "dash",
          color = "gray"
        )
      )
    ),
    
    legend = list(title = list(text = "Region"))
  ) %>%
  
  animation_opts(
    frame = 700,
    transition = 0,
    redraw = TRUE
  ) %>%
  
  animation_slider(
    currentvalue = list(prefix = "Year: ")
  )


# ------------------------------------------------------------
# 12. Figure 3: Compare spending trends across selected states
# ------------------------------------------------------------

# Edit this vector to compare other states.
selected_states <- c(
  "Pennsylvania",
  "New York",
  "California",
  "Texas",
  "Massachusetts",
  "Florida"
)

trend_data <- state_year %>%
  filter(state %in% selected_states) %>%
  arrange(state, year)

fig3 <- plot_ly(
  data = trend_data,
  
  x = ~year,
  y = ~total_pce_per_capita,
  
  color = ~state,
  
  type = "scatter",
  mode = "lines+markers",
  
  text = ~paste0(
    state,
    "<br>Year: ", year,
    "<br>Annual spending per person: $",
    format(
      round(total_pce_per_capita),
      big.mark = ",",
      trim = TRUE
    )
  ),
  
  hovertemplate = "%{text}<extra></extra>"
) %>%
  
  layout(
    title = "How Has Spending per Person Changed?",
    
    xaxis = list(
      title = "Year",
      rangeslider = list(visible = TRUE)
    ),
    
    yaxis = list(
      title = "Annual spending per person (current USD)",
      tickprefix = "$",
      separatethousands = TRUE
    ),
    
    legend = list(title = list(text = "State"))
  )


# ------------------------------------------------------------
# 13. Display the interactive charts
# ------------------------------------------------------------

# Run these lines individually in RStudio.
# Each chart appears in the Viewer pane.

fig1

htmlwidgets::saveWidget(
  fig1,
  "output/figure1_rankings.html",
  selfcontained = FALSE
)

browseURL(
  normalizePath("output/figure1_rankings.html")
)

plotly::plot_ly(
  x = c(1, 2, 3),
  y = c(2, 4, 3),
  type = "bar"
)
















fig2
fig3


# ------------------------------------------------------------
# 14. Export data and interactive HTML files
# ------------------------------------------------------------

write_csv(
  consumption,
  "output/consumption_long.csv"
)

write_csv(
  state_year,
  "output/state_year_analysis.csv"
)

write_csv(
  pivot_spending,
  "output/pivot_spending.csv"
)

write_csv(
  pivot_shares,
  "output/pivot_shares.csv"
)

write_csv(
  pivot_region,
  "output/pivot_region.csv"
)

# selfcontained = FALSE avoids requiring Pandoc.
# Keep each HTML file with its corresponding dependencies folder.

saveWidget(
  fig1,
  "output/figure1_rankings.html",
  selfcontained = FALSE
)

saveWidget(
  fig2,
  "output/figure2_income_prices.html",
  selfcontained = FALSE
)

saveWidget(
  fig3,
  "output/figure3_trends.html",
  selfcontained = FALSE
)


# ------------------------------------------------------------
# 15. Generate a compact report to send back for review
# ------------------------------------------------------------

latest <- state_year %>%
  filter(year == selected_year)

report <- capture.output({
  
  cat("DATA VALIDATION\n")
  cat("States:", n_distinct(state_year$state), "\n")
  cat("Years:", start_year, "-", end_year, "\n")
  cat("State-year rows:", nrow(state_year), "\n")
  
  cat("\nMISSING VALUES\n")
  print(
    state_year %>%
      summarise(
        across(
          c(
            total_pce_per_capita,
            income_per_capita,
            population,
            rpp
          ),
          ~sum(is.na(.x))
        )
      )
  )
  
  cat("\nTOP 10 STATES BY CONSUMPTION PER PERSON\n")
  print(
    latest %>%
      arrange(desc(total_pce_per_capita)) %>%
      select(
        state,
        total_pce_per_capita,
        income_per_capita,
        rpp
      ) %>%
      head(10),
    width = Inf
  )
  
  cat("\nLARGEST IMPROVEMENTS IN INCOME RANK\n")
  print(
    latest %>%
      arrange(desc(rank_improvement)) %>%
      select(
        state,
        nominal_income_rank,
        adjusted_income_rank,
        rank_improvement
      ) %>%
      head(10),
    width = Inf
  )
  
  cat("\nPEARSON CORRELATIONS IN SELECTED YEAR\n")
  
  print(
    latest %>%
      summarise(
        income_vs_consumption = cor(
          income_per_capita,
          total_pce_per_capita
        ),
        
        income_vs_prices = cor(
          income_per_capita,
          rpp
        )
      )
  )
  
  cat("\nPENNSYLVANIA SNAPSHOT\n")
  print(
    latest %>%
      filter(state == "Pennsylvania"),
    width = Inf
  )
})

writeLines(
  report,
  "output/check_results.txt"
)

cat(paste(report, collapse = "\n"))

cat("\n\nFiles saved in:\n")
cat(normalizePath("output"), "\n")

