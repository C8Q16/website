library(tidyverse)

cps <- read_csv(
  "/Users/yangyixin/Desktop/研一上/AEDS6400/blog3/cps_00002.csv.gz"
)
range(cps$YEAR)

table(cps$YEAR)

range(cps$MONTH)

table(cps$MONTH[cps$YEAR == 2026])
cps <- cps %>%
  filter(YEAR >= 2020, YEAR <= 2025)

table(cps$SEX, useNA = "ifany")

table(cps$LABFORCE, useNA = "ifany")

table(cps$EDUC, useNA = "ifany")

summary(cps$AGE)

summary(cps$WTFINL)
cps_clean <- cps %>%
  filter(
    AGE >= 16,
    LABFORCE %in% c(1, 2),
    !is.na(WTFINL),
    WTFINL > 0
  )

cps_clean <- cps_clean %>%
  mutate(
    in_labor_force = if_else(LABFORCE == 2, 1, 0)
  )

dim(cps_clean)

table(cps_clean$LABFORCE)

summary(cps_clean$AGE)
cps_clean <- cps_clean %>%
  mutate(
    age_group = case_when(
      AGE < 25 ~ "16–24",
      AGE < 35 ~ "25–34",
      AGE < 45 ~ "35–44",
      AGE < 55 ~ "45–54",
      AGE < 65 ~ "55–64",
      TRUE ~ "65+"
    ),
    age_group = factor(
      age_group,
      levels = c("16–24", "25–34", "35–44",
                 "45–54", "55–64", "65+")
    )
  )
table(cps_clean$age_group)

lfpr_age <- cps_clean %>%
  group_by(age_group) %>%
  summarise(
    lfpr = sum(WTFINL * in_labor_force) / sum(WTFINL),
    .groups = "drop"
  ) %>%
  mutate(
    lfpr_percent = lfpr * 100
  )

lfpr_age

#Chart1
ggplot(lfpr_age, aes(x = age_group, y = lfpr_percent)) +
  geom_col() +
  labs(
    title = "Labor Force Participation Rate by Age Group",
    subtitle = "U.S., 2020–2025",
    x = "Age group",
    y = "Labor force participation rate (%)"
  ) +
  theme_minimal()

#chart2
cps_clean <- cps_clean %>%
  mutate(
    sex_label = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"
    )
  )

lfpr_age_sex <- cps_clean %>%
  group_by(age_group, sex_label) %>%
  summarise(
    lfpr = sum(WTFINL * in_labor_force) / sum(WTFINL),
    .groups = "drop"
  ) %>%
  mutate(
    lfpr_percent = lfpr * 100
  )

lfpr_age_sex

ggplot(
  lfpr_age_sex,
  aes(
    x = age_group,
    y = lfpr_percent,
    group = sex_label,
    color = sex_label
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0(round(lfpr_percent, 1), "%")),
    vjust = -0.8,
    size = 3.5
  ) +
  labs(
    title = "Labor Force Participation Rate by Age and Gender",
    subtitle = "U.S., 2020–2025",
    x = "Age group",
    y = "Labor force participation rate (%)",
    color = "Gender"
  ) +
  theme_minimal()


#chart3
table(cps_clean$EDUC)

cps_clean <- cps_clean %>%
  mutate(
    education_group = case_when(
      EDUC %in% c(2, 10, 20, 30, 40, 50, 60, 71) ~
        "Less than high school",
      
      EDUC == 73 ~
        "High school diploma",
      
      EDUC %in% c(81, 91, 92) ~
        "Some college / associate",
      
      EDUC %in% c(111, 123, 124, 125) ~
        "Bachelor's degree or higher"
    ),
    
    education_group = factor(
      education_group,
      levels = c(
        "Less than high school",
        "High school diploma",
        "Some college / associate",
        "Bachelor's degree or higher"
      )
    )
  )
table(cps_clean$education_group, useNA = "ifany")

lfpr_age_edu <- cps_clean %>%
  group_by(age_group, education_group) %>%
  summarise(
    lfpr = sum(WTFINL * in_labor_force) / sum(WTFINL),
    .groups = "drop"
  ) %>%
  mutate(
    lfpr_percent = lfpr * 100
  )

lfpr_age_edu
print(lfpr_age_edu, n = 24)

ggplot(
  lfpr_age_edu,
  aes(
    x = education_group,
    y = age_group,
    fill = lfpr_percent
  )
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = paste0(round(lfpr_percent, 1), "%")),
    size = 4
  ) +
  scale_fill_viridis_c(
    name = "LFPR (%)",
    limits = c(0, 100)
  ) +
  labs(
    title = "Labor Force Participation Rate by Age and Education",
    subtitle = "U.S., 2020–2025",
    x = "Educational attainment",
    y = "Age group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

#chart4 Boxplot — Age Distribution by Education
ggplot(
  cps_clean,
  aes(
    x = education_group,
    y = AGE
  )
) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Educational Attainment",
    subtitle = "U.S. CPS, 2020–2025",
    x = "Educational attainment",
    y = "Age"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

#chart5 Dot Plot — Age × Education
ggplot(
  lfpr_age_edu,
  aes(
    x = education_group,
    y = lfpr_percent,
    color = age_group
  )
) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Labor Force Participation by Education and Age",
    subtitle = "U.S., 2020–2025",
    x = "Educational attainment",
    y = "Labor force participation rate (%)",
    color = "Age group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

#chart6 Density plot
ggplot(
  cps_clean,
  aes(
    x = AGE,
    weight = WTFINL,
    fill = sex_label
  )
) +
  geom_density(
    alpha = 0.4
  ) +
  labs(
    title = "Age Distribution by Gender",
    subtitle = "Weighted CPS observations, 2020–2025",
    x = "Age",
    y = "Weighted density",
    fill = "Gender"
  ) +
  theme_minimal()
