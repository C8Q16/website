install.packages(c(
  "tidycensus",
  "sf",
  "dplyr",
  "ggplot2",
  "scales",
  "readr"
))



# ============================================================
# Blog Post 5: The Geography of Housing Affordability
# Study area: Philadelphia, Pennsylvania
# Data: 2020-2024 ACS 5-year estimates
# ============================================================

library(tidycensus)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)

# Create a folder for exported results
dir.create("output", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Download ACS data and Census Tract boundaries
# ------------------------------------------------------------

acs_vars <- c(
  total_rent = "B25070_001",
  rent_30_34 = "B25070_007",
  rent_35_39 = "B25070_008",
  rent_40_49 = "B25070_009",
  rent_50_plus = "B25070_010",
  rent_not_computed = "B25070_011",
  median_income = "B19013_001"
)

philly_raw <- tidycensus::get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  variables = acs_vars,
  year = 2024,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
)

# Check whether the expected estimate columns were returned
expected_cols <- paste0(names(acs_vars), "E")
missing_cols <- setdiff(expected_cols, names(philly_raw))

if (length(missing_cols) > 0) {
  print(names(philly_raw))
  stop(
    "Some expected columns are missing. ",
    "Please check the variable names and ACS download."
  )
}

# ------------------------------------------------------------
# 2. Clean data and construct the main indicators
# ------------------------------------------------------------

philly <- philly_raw %>%
  mutate(
    # Number of renter households with a computable rent ratio
    rent_eligible = total_rentE - rent_not_computedE,
    
    # Number of renter households spending >= 30% of income
    burdened_renters =
      rent_30_34E +
      rent_35_39E +
      rent_40_49E +
      rent_50_plusE,
    
    # Rent burden rate, between 0 and 1
    rent_burden_share = if_else(
      !is.na(rent_eligible) & rent_eligible > 0,
      burdened_renters / rent_eligible,
      NA_real_
    ),
    
    # Treat nonpositive or unavailable income estimates as missing
    median_income = if_else(
      median_incomeE > 0,
      as.numeric(median_incomeE),
      NA_real_
    )
  )

# Prepare separate datasets for each figure
rent_map <- philly %>%
  filter(is.finite(rent_burden_share))

income_map <- philly %>%
  filter(is.finite(median_income))

scatter_data <- philly %>%
  filter(
    is.finite(rent_burden_share),
    is.finite(median_income)
  )

# ------------------------------------------------------------
# 3. Figure 1: Map of rent burden across Philadelphia
# ------------------------------------------------------------


p1 <- ggplot(philly) +
  geom_sf(
    aes(fill = rent_burden_share),
    color = "white",
    linewidth = 0.08
  ) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = scales::label_percent(accuracy = 1),
    na.value = "grey85"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "The Geography of Rent Burden in Philadelphia",
    subtitle = paste(
      "Share of renter households spending at least",
      "30% of income on gross rent"
    ),
    fill = "Rent burden\nrate",
    caption = paste(
      "Source: U.S. Census Bureau,",
      "2020-2024 ACS 5-year estimates, Table B25070.",
      "Gray areas indicate unavailable estimates."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "right",
    plot.caption.position = "plot",
    plot.margin = margin(12, 15, 12, 15)
  )

print(p1)




# ------------------------------------------------------------
# 4. Figure 2: Map of median household income
# ------------------------------------------------------------


p2 <- ggplot(philly) +
  geom_sf(
    aes(fill = median_income),
    color = "white",
    linewidth = 0.08
  ) +
  scale_fill_viridis_c(
    option = "cividis",
    breaks = c(25000, 50000, 75000, 100000, 150000, 200000),
    labels = scales::label_dollar(accuracy = 1000),
    na.value = "grey85"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "The Geography of Household Income",
    subtitle = paste(
      "Median household income by Census Tract;",
      "2024 inflation-adjusted dollars"
    ),
    fill = "Median\nincome",
    caption = paste(
      "Source: U.S. Census Bureau, 2020-2024 ACS",
      "5-year estimates, Table B19013.",
      "Gray areas indicate unavailable estimates."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "right",
    plot.caption.position = "plot",
    plot.margin = margin(15, 25, 15, 25)
  )

print(p2)


# ------------------------------------------------------------
# 5. Figure 3: Relationship between income and rent burden
# ------------------------------------------------------------

p3 <- ggplot(
  scatter_data,
  aes(x = median_income, y = rent_burden_share)
) +
  geom_point(
    color = "#6C5CE7",
    alpha = 0.65,
    size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D55E00",
    linewidth = 0.9
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(accuracy = 10000)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(
    title = "Household Income and Rent Burden",
    subtitle = "Each point represents one Philadelphia Census Tract",
    x = "Median household income",
    y = "Rent burden rate",
    caption = paste(
      "Source: 2020-2024 ACS 5-year estimates.",
      "The fitted line describes an association, not causation."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption = element_text(size = 8, color = "grey40")
  )

print(p3)

# ------------------------------------------------------------
# 6. Save the three figures
# ------------------------------------------------------------

ggsave(
  "output/figure1_rent_burden_map.png",
  plot = p1,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  "output/figure2_median_income_map.png",
  plot = p2,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  "output/figure3_income_vs_rent_burden.png",
  plot = p3,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 7. Calculate summary statistics
# ------------------------------------------------------------

rent_valid <- philly %>%
  filter(
    is.finite(rent_eligible),
    rent_eligible > 0,
    is.finite(burdened_renters)
  )

# Aggregate citywide estimate, weighted by renter-household counts
citywide_share <-
  sum(rent_valid$burdened_renters) /
  sum(rent_valid$rent_eligible)

# Median of tract-level rates; this is NOT the citywide rate
median_tract_share <- median(
  rent_map$rent_burden_share,
  na.rm = TRUE
)

# Pearson correlation across tracts
income_correlation <- cor(
  scatter_data$median_income,
  scatter_data$rent_burden_share,
  method = "pearson"
)

cat("\n========== SUMMARY RESULTS ==========\n")
cat("Total Census Tracts downloaded:", nrow(philly), "\n")
cat("Tracts with rent-burden data:", nrow(rent_map), "\n")
cat("Tracts with both indicators:", nrow(scatter_data), "\n")

cat(
  "Aggregate rent-burden rate:",
  scales::percent(citywide_share, accuracy = 0.1), "\n"
)

cat(
  "Median tract-level rent-burden rate:",
  scales::percent(median_tract_share, accuracy = 0.1), "\n"
)

cat(
  "Pearson correlation (income vs. rent burden):",
  round(income_correlation, 3), "\n"
)

# ------------------------------------------------------------
# 8. Export a table of the 10 highest rent-burden tracts
# ------------------------------------------------------------

top10 <- rent_map %>%
  sf::st_drop_geometry() %>%
  arrange(desc(rent_burden_share)) %>%
  slice_head(n = 10) %>%
  mutate(
    rent_burden_percent = round(rent_burden_share * 100, 1),
    median_household_income = round(median_income),
    estimated_burdened_renters = round(burdened_renters),
    eligible_renter_units = round(rent_eligible)
  ) %>%
  select(
    NAME,
    GEOID,
    rent_burden_percent,
    median_household_income,
    estimated_burdened_renters,
    eligible_renter_units
  )

cat("\n========== TOP 10 TRACTS ==========\n")
print(as.data.frame(top10), row.names = FALSE)

write_csv(
  top10,
  "output/top10_tracts_rent_burden.csv"
)

# Save the tract data without the geometry column
write_csv(
  philly %>% sf::st_drop_geometry(),
  "output/philadelphia_tract_data.csv"
)

cat("\nAll figures and CSV files were saved to the output folder.\n")
file.exists("output/top10_tracts_rent_burden.csv")
file.exists("output/philadelphia_tract_data.csv")






top10_more_stable <- philly %>%
  filter(
    is.finite(rent_burden_share),
    rent_eligible >= 100
  ) %>%
  sf::st_drop_geometry() %>%
  arrange(desc(rent_burden_share)) %>%
  slice_head(n = 10) %>%
  transmute(
    NAME,
    GEOID,
    rent_burden_percent = round(rent_burden_share * 100, 1),
    median_household_income = round(median_income),
    estimated_burdened_renters = round(burdened_renters),
    eligible_renter_units = round(rent_eligible)
  )

print(as.data.frame(top10_more_stable), row.names = FALSE)

cor(
  scatter_data$median_income,
  scatter_data$rent_burden_share,
  method = "spearman"
)


# Check the ACS margins of error for the main rent-burden inputs
philly %>%
  sf::st_drop_geometry() %>%
  filter(
    GEOID %in% top10_more_stable$GEOID
  ) %>%
  transmute(
    NAME,
    GEOID,
    total_rent = total_rentE,
    total_rent_moe = total_rentM,
    rent_30_34 = rent_30_34E,
    rent_35_39 = rent_35_39E,
    rent_40_49 = rent_40_49E,
    rent_50_plus = rent_50_plusE,
    not_computed = rent_not_computedE,
    rent_burden_share,
    rent_burden_moe_inputs_available =
      !is.na(total_rentM)
  ) %>%
  print()



library(dplyr)
library(sf)

# Approximate the 90% MOE for the derived rent-burden rate
philly_uncertainty <- philly %>%
  mutate(
    # MOE for the estimated number of renters spending >= 30%
    burdened_renters_moe = sqrt(
      rent_30_34M^2 +
        rent_35_39M^2 +
        rent_40_49M^2 +
        rent_50_plusM^2
    ),
    
    # Approximate MOE for eligible renters:
    # total renters minus renters whose rent/income ratio
    # could not be computed
    eligible_renters_moe = sqrt(
      total_rentM^2 +
        rent_not_computedM^2
    ),
    
    # Formula for the derived proportion
    ratio_term =
      burdened_renters_moe^2 -
      rent_burden_share^2 * eligible_renters_moe^2,
    
    # Use the Census Bureau's fallback formula if the
    # expression under the square root is negative
    rent_burden_moe = case_when(
      is.na(rent_burden_share) |
        is.na(burdened_renters_moe) |
        is.na(eligible_renters_moe) |
        rent_eligible <= 0 ~ NA_real_,
      
      ratio_term >= 0 ~
        sqrt(ratio_term) / rent_eligible,
      
      TRUE ~
        sqrt(
          burdened_renters_moe^2 +
            (rent_burden_share * eligible_renters_moe)^2
        ) / rent_eligible
    ),
    
    # Approximate 90% confidence interval, expressed as a share
    rent_burden_ci_low = pmax(
      0,
      rent_burden_share - rent_burden_moe
    ),
    
    rent_burden_ci_high = pmin(
      1,
      rent_burden_share + rent_burden_moe
    )
  )

# Examine the 10 highest-rate tracts with at least
# 100 estimated eligible renter households
top10_ci <- philly_uncertainty %>%
  filter(
    is.finite(rent_burden_share),
    is.finite(rent_burden_moe),
    rent_eligible >= 100
  ) %>%
  arrange(desc(rent_burden_share)) %>%
  slice_head(n = 10) %>%
  sf::st_drop_geometry() %>%
  transmute(
    NAME,
    GEOID,
    eligible_renters = round(rent_eligible),
    rent_burden_rate_pct =
      round(rent_burden_share * 100, 1),
    approximate_moe_percentage_points =
      round(rent_burden_moe * 100, 1),
    ci_lower_pct =
      round(rent_burden_ci_low * 100, 1),
    ci_upper_pct =
      round(rent_burden_ci_high * 100, 1)
  )

print(as.data.frame(top10_ci), row.names = FALSE)

