# Carga los paquetes a usar, junto con la versión limpia del dataframe ####
# Este es el documento de análisis final 

source("./code/cleaning.r")

# Prototipo inicial del código provisto por Perplexity

# Prepare analysis dataset with all covariates
analysis_df <- main_df |> 
  # Select relevant columns
  select(id, gender, dob, hypertension, diabetes, 
         contains("hosp"), contains("period_")) |> 
  
  # Convert to long format for periods
  pivot_longer(
    cols = matches("hospital_(start|end|cause|readmission)_[1-9]"),
    names_to = c(".value", "adm_number"),
    names_pattern = "(.+?)_([1-9])",
    values_drop_na = TRUE
  ) |> 
  
  # Categorize hospitalizations by flood period
  mutate(
    period = case_when(
      hospital_start > period_predana_start & hospital_end < period_predana_end ~ "Pre-DANA",
      hospital_start > period_postdana_start & hospital_end < period_postdana_end ~ "Post-DANA",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(period)) |> 
  
  # Add patient characteristics
  left_join(
    distinct(select(main_df, id, gender, dob, hypertension, diabetes)),
    by = "id"
  ) |> 
  
  # Create analysis variables
  mutate(
    age_group = cut(age, breaks = c(0, 18, 65, Inf), 
                    labels = c("Pediatric", "Adult", "Elderly")),
    comorbidity = case_when(
      hypertension & diabetes ~ "Both",
      hypertension ~ "Hypertension",
      diabetes ~ "Diabetes",
      TRUE ~ "None"
    )
  )

# 1. Overall Hospitalization Rates by Period and Cause
tbl_overall <- analysis_df |> 
  tbl_strata(
    strata = period,
    .tbl_fun =
      ~ .x |>
      tbl_summary(
        include = c(hospital_cause, age_group, gender, comorbidity),
        by = hospital_cause,
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        label = list(
          age_group ~ "Age Group",
          comorbidity ~ "Comorbidity Status"
        )
      ) |>
      add_p(test = list(all_categorical() ~ "chisq.test")),
    .header = "**{strata}**"
  )

# 2. Risk Ratio Analysis Using Negative Binomial Regression
model <- glm.nb(
  hosp_count ~ period + hospital_cause + age_group + gender + comorbidity +
    offset(log(person_time)),
  data = analysis_df |> 
    group_by(id, period) |> 
    mutate(hosp_count = n()) |> 
    ungroup() |> 
    distinct(id, period, .keep_all = TRUE)
)

# Format results with gtsummary
tbl_model <- tbl_regression(
  model,
  exponentiate = TRUE,
  label = list(
    period ~ "Flood Period",
    hospital_cause ~ "Cause of Hospitalization",
    age_group ~ "Age Group",
    comorbidity ~ "Comorbidity Status"
  )
) |> 
  add_global_p() |> 
  modify_header(label ~ "**Variable**")

# 3. Subgroup Analysis for High-Risk Groups
high_risk_table <- analysis_df |> 
  filter(comorbidity %in% c("Hypertension", "Diabetes", "Both")) |> 
  tbl_strata(
    strata = comorbidity,
    .tbl_fun =
      ~ .x |>
      tbl_summary(
        include = c(period, hospital_cause),
        by = period,
        statistic = list(all_categorical() ~ "{n} ({p}%)")
      ) |>
      add_p(test = list(all_categorical() ~ "fisher.test")),
    .header = "**{strata}**"
  )

# 4. Visualization of Key Results
ggplot(analysis_df |> 
         count(period, hospital_cause, age_group), 
       aes(x = hospital_cause, y = n, fill = period)) +
  geom_col(position = "dodge") +
  facet_wrap(~age_group, scales = "free_y") +
  labs(title = "Hospitalizations by Cause, Age Group, and Flood Period",
       x = "Cause of Hospitalization",
       y = "Number of Admissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Output Key Tables
list(
  Overall_Rates = tbl_overall,
  Risk_Factors = tbl_model,
  High_Risk_Groups = high_risk_table
)
