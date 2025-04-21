# Project 4 Part 2 – Louis Williams

# 0. Load packages
library(tidyverse)   # data wrangling & ggplot2
library(ggplot2)     # for clarity in plotting



# 1. Load CPS microdata (compressed)
data <- read.csv("cps_00001.csv.gz")

# 2. Filter to working‑age population (16+)
data_filtered <- data %>%
  filter(AGE >= 16)

# 3. Classify labor‐market state from EMPSTAT codes
data_classified <- data_filtered %>%
  mutate(
    labor_state = case_when(
      EMPSTAT %in% c(10,12)           ~ "E",  # Employed
      EMPSTAT %in% c(20,21,22)        ~ "U",  # Unemployed
      EMPSTAT %in% c(30,31,32,34,36)  ~ "N",  # Not in labor force
      TRUE                            ~ NA_character_
    )
  )

# 4. Compute monthly aggregates & rates
monthly_indicators <- data_classified %>%
  group_by(YEAR, MONTH) %>%
  summarize(
    total_pop        = sum(WTFINL, na.rm = TRUE),                   # population weight sum
    labor_force      = sum(WTFINL[labor_state %in% c("E","U")]),    # E+U
    unemployed       = sum(WTFINL[labor_state == "U"]),             # U only
    not_in_lf        = sum(WTFINL[labor_state == "N"]),             # N only
    .groups = "drop"
  ) %>%
  mutate(
    unemployment_rate     = unemployed / labor_force,               # U / (E+U)
    nonparticipation_rate = not_in_lf / total_pop,                  # N / total
    date                  = as.Date(paste(YEAR, MONTH, "01", sep = "-"))
  )

# 5. Build panel for month‑to‑month transitions
panel_transitions <- data_classified %>%
  arrange(CPSIDP, YEAR, MONTH) %>%                         # order by person and time
  group_by(CPSIDP) %>%
  mutate(
    next_state = lead(labor_state),                        # next month’s state
    next_year  = lead(YEAR),
    next_month = lead(MONTH)
  ) %>%
  ungroup() %>%
  filter(
    (next_year == YEAR & next_month == MONTH + 1) |         # consecutive month
      (YEAR + 1 == next_year & MONTH == 12 & next_month == 1)
  ) %>%
  filter(!is.na(next_state))

# 6. Compute transition probabilities
transition_probs <- panel_transitions %>%
  count(labor_state, next_state, name = "n") %>%            # count flows
  group_by(labor_state) %>%
  mutate(probability = n / sum(n)) %>%                      # convert to probability
  ungroup()

# 7. Prepare monthly transition‐rate series
monthly_transition_probs <- panel_transitions %>%
  group_by(YEAR, MONTH, labor_state, next_state) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(YEAR, MONTH, labor_state) %>%
  mutate(
    prob = n / sum(n),
    date = as.Date(paste(YEAR, MONTH, "01", sep = "-"))
  ) %>%
  ungroup()

# 8. Plot selected transitions over time (E→U, U→E, N→E, U→N)
transition_flows <- monthly_transition_probs %>%
  filter(paste(labor_state, next_state, sep = " → ") %in%
           c("E → U", "U → E", "N → E", "U → N")) %>%
  mutate(transition = paste(labor_state, next_state, sep = " → "))

ggplot(transition_flows, aes(x = date, y = prob, color = transition)) +
  geom_line(size = 1.2) +
  labs(
    title = "Monthly Transition Probabilities Between States",
    x = "Date", y = "Probability", color = "Transition"
  ) +
  theme_minimal()

# 9. Plot labor‐market rates together
monthly_states_plot <- monthly_indicators %>%
  transmute(
    date,
    Employment         = (labor_force - unemployed) / total_pop,
    Unemployment       = unemployment_rate,
    Nonparticipation   = nonparticipation_rate
  ) %>%
  pivot_longer(-date, names_to = "Indicator", values_to = "Rate")

ggplot(monthly_states_plot, aes(x = date, y = Rate, color = Indicator)) +
  geom_line(size = 1.2) +
  labs(
    title = "Labor Market Rates (2018–2024)",
    x = "Date", y = "Rate"
  ) +
  theme_minimal()
