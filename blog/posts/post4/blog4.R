# ============================================================
# Computational Methods for Economists
# Blog Post 4: Dataset of Choice
#
# Research Question:
# How has housing affordability changed in the United States
# since 2000?
#
# Data source: Federal Reserve Bank of St. Louis (FRED)
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

# Run this ONCE if packages are not installed:
# install.packages(c("fredr", "tidyverse", "lubridate", "scales"))

library(fredr)
library(tidyverse)
library(lubridate)
library(scales)


# ============================================================
# 2. FRED API key
# ============================================================

# Option A: directly enter your API key
# fredr_set_key("YOUR_FRED_API_KEY")

# Option B: use an environment variable
fredr_set_key(Sys.getenv("FRED_API_KEY"))


# ============================================================
# 3. Define time period and FRED series
# ============================================================

start_date <- as.Date("2000-01-01")
end_date   <- as.Date("2025-12-31")

series_ids <- c(
  home_price = "MSPUS",
  income     = "MEHOINUSA672N",
  mortgage   = "MORTGAGE30US",
  cpi        = "CPIAUCSL"
)


# ============================================================
# 4. Download data from FRED
# ============================================================

# 4.1 Median sales price of new houses sold
home_price_raw <- fredr(
  series_id = series_ids["home_price"],
  observation_start = start_date,
  observation_end = end_date
)

# 4.2 Real median household income
income_raw <- fredr(
  series_id = series_ids["income"],
  observation_start = start_date,
  observation_end = end_date
)

# 4.3 30-year fixed mortgage rate
mortgage_raw <- fredr(
  series_id = series_ids["mortgage"],
  observation_start = start_date,
  observation_end = end_date
)

# 4.4 CPI
cpi_raw <- fredr(
  series_id = series_ids["cpi"],
  observation_start = start_date,
  observation_end = end_date
)


# ============================================================
# 5. Quick checks of raw data
# ============================================================

head(home_price_raw)
head(income_raw)
head(mortgage_raw)
head(cpi_raw)

nrow(home_price_raw)
nrow(income_raw)
nrow(mortgage_raw)
nrow(cpi_raw)


# ============================================================
# 6. Convert all series to annual frequency
# ============================================================

# Home price: quarterly -> annual average
home_price_annual <- home_price_raw %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    home_price_nominal = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# Mortgage rate: weekly -> annual average
mortgage_annual <- mortgage_raw %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    mortgage_rate = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# CPI: monthly -> annual average
cpi_annual <- cpi_raw %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    cpi = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# Income is already annual
income_annual <- income_raw %>%
  mutate(year = year(date)) %>%
  select(year, income_real = value)


# ============================================================
# 7. Merge all annual data
# ============================================================

housing_data <- home_price_annual %>%
  left_join(income_annual, by = "year") %>%
  left_join(mortgage_annual, by = "year") %>%
  left_join(cpi_annual, by = "year") %>%
  filter(year >= 2000, year <= 2025)


# Check final dataset
glimpse(housing_data)

nrow(housing_data)
range(housing_data$year)


# ============================================================
# 8. Check for missing values
# ============================================================

housing_data %>%
  summarise(
    missing_home_price = sum(is.na(home_price_nominal)),
    missing_income     = sum(is.na(income_real)),
    missing_mortgage   = sum(is.na(mortgage_rate)),
    missing_cpi        = sum(is.na(cpi))
  )


# ============================================================
# 9. Convert nominal home prices to real 2025 dollars
# ============================================================

# 2025 annual-average CPI
cpi_2025 <- cpi_annual %>%
  filter(year == 2025) %>%
  pull(cpi)

housing_data <- housing_data %>%
  mutate(
    home_price_real = home_price_nominal * cpi_2025 / cpi
  )


# Quick check
housing_data %>%
  select(
    year,
    home_price_nominal,
    home_price_real,
    income_real
  ) %>%
  head()


# ============================================================
# 10. Create 2000 = 100 indices
# ============================================================

home_price_2000 <- housing_data %>%
  filter(year == 2000) %>%
  pull(home_price_real) %>%
  first()

income_2000 <- housing_data %>%
  filter(year == 2000) %>%
  pull(income_real) %>%
  first()

housing_data <- housing_data %>%
  mutate(
    home_price_index = home_price_real / home_price_2000 * 100,
    income_index     = income_real / income_2000 * 100
  )


# Check that both indices equal 100 in 2000
housing_data %>%
  filter(year == 2000) %>%
  select(year, home_price_index, income_index)


# ============================================================
# 11. Create home price-to-income ratio
# ============================================================

housing_data <- housing_data %>%
  mutate(
    price_to_income = home_price_real / income_real
  )


# Check
housing_data %>%
  select(year, home_price_real, income_real, price_to_income) %>%
  head()


# ============================================================
# 12. Calculate illustrative mortgage payment burden
# ============================================================

# Assumptions:
# - 20% down payment
# - 80% loan-to-value ratio
# - 30-year fixed-rate mortgage
# - 360 monthly payments
# - excludes property taxes, insurance, and maintenance

housing_data <- housing_data %>%
  mutate(
    loan_amount = 0.80 * home_price_nominal,
    
    monthly_rate = (mortgage_rate / 100) / 12,
    
    monthly_payment =
      loan_amount *
      monthly_rate *
      (1 + monthly_rate)^360 /
      ((1 + monthly_rate)^360 - 1),
    
    annual_mortgage_payment =
      12 * monthly_payment,
    
    # Convert the nominal mortgage payment
    # into 2025 dollars
    annual_mortgage_payment_real =
      annual_mortgage_payment * cpi_2025 / cpi,
    
    mortgage_burden_pct =
      annual_mortgage_payment_real / income_real * 100
  )


# Check mortgage burden
housing_data %>%
  select(
    year,
    home_price_nominal,
    mortgage_rate,
    annual_mortgage_payment_real,
    income_real,
    mortgage_burden_pct
  ) %>%
  head()


# ============================================================
# 13. Figure 1
# Real home prices vs. real median household income
# ============================================================

fig1_data <- housing_data %>%
  select(
    year,
    home_price_index,
    income_index
  ) %>%
  pivot_longer(
    cols = c(home_price_index, income_index),
    names_to = "variable",
    values_to = "index"
  ) %>%
  mutate(
    variable = recode(
      variable,
      home_price_index = "Real home price",
      income_index = "Real median household income"
    )
  )


fig1 <- ggplot(
  fig1_data,
  aes(
    x = year,
    y = index,
    linetype = variable
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Real Home Prices Have Outpaced Household Income",
    subtitle = "United States, 2000 = 100",
    x = NULL,
    y = "Index (2000 = 100)",
    linetype = NULL,
    caption = "Sources: U.S. Census Bureau, Freddie Mac, and U.S. Bureau of Labor Statistics via FRED."
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )

fig1


# ============================================================
# 14. Figure 2
# Home price-to-income ratio
# ============================================================

fig2 <- ggplot(
  housing_data,
  aes(
    x = year,
    y = price_to_income
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "U.S. Housing Has Become More Expensive Relative to Income",
    subtitle = "Real median new-home price relative to real median household income",
    x = NULL,
    y = "Home price-to-income ratio",
    caption = "Sources: U.S. Census Bureau and U.S. Bureau of Labor Statistics via FRED."
  ) +
  theme_minimal()

fig2


# ============================================================
# 15. Figure 3
# Illustrative mortgage payment burden
# ============================================================

fig3 <- ggplot(
  housing_data,
  aes(
    x = year,
    y = mortgage_burden_pct
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Mortgage Financing Costs Add to Housing Affordability Pressure",
    subtitle = "Illustrative annual mortgage payment as a share of median household income",
    x = NULL,
    y = "Mortgage payment / household income (%)",
    caption = "Assumes 20% down payment and a 30-year fixed mortgage; excludes taxes, insurance, and maintenance."
  ) +
  theme_minimal()

fig3


# ============================================================
# 16. Optional: display the key years
# ============================================================

housing_data %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020, 2025)) %>%
  select(
    year,
    home_price_real,
    income_real,
    home_price_index,
    income_index,
    price_to_income,
    mortgage_rate,
    mortgage_burden_pct
  )

nrow(housing_data)
range(housing_data$year)
