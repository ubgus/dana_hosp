# Carga los paquetes a usar, junto con la versión limpia del dataframe ####
# Este es el documento de análisis final 

source("./code/cleaning.r")

# Prototipo inicial del código provisto por Perplexity (comentarios en inglés)

# Crear tabla para comparar datos demográficos y comorbilidades ####
demogr_df <- main_df |> 
  select(id, gender, birth_date, htn, diabetes, diabetes_type, contains("p1"), contains("p2")) |> 
  pivot_longer(
    cols = matches("_p[12]"),
    names_to = c(".value", "period"),
    names_pattern = "(.+?)_(p[12])") |> 

  # Eliminar entradas duplicadas de pacientes que solo están presentes un periodo y aquellos que iniciaron después del periodo
  filter(!is.na(clinic) & !is.na(va)) |>
  
  # Traducir parámetros y calcular la edad en base a la fecha de inicio de cada periodo
  mutate(
    period = factor(case_when(
      period == "p1" ~ "Pre-DANA",
      period == "p2" ~ "Post-DANA"),
      levels = c("Pre-DANA", "Post-DANA")),
    diabetes_type = case_match(
      diabetes_type,
      "Type 1" ~ "Tipo 1",
      "Type 2" ~ "Tipo 2"),
    va = case_match(
      va,
      "Autologous AV Fistula" ~ "Fistula AV Nativa",
      "AV Graft" ~ "Fistula Protésica",
      "Tunneled catheter" ~ "Catéter Tunelizado"),
    age = if_else(
      period == "Pre-DANA",
      time_length(interval(birth_date, period_predana_start), "years"),
      time_length(interval(birth_date, period_postdana_start), "years")))

# Genera tabla comparativa demográfica
demogr_tbl <- demogr_df |> 
  tbl_summary(
    by = period,
    include = c(age, gender, htn, diabetes, diabetes_type, charlson, karnofsky, va),
    label = list(
      age ~ "Edad",
      gender ~ "Mujeres",
      htn ~ "Hipertensión",
      diabetes ~ "Diabetes Mellitus",
      diabetes_type ~ "Tipo de Diabetes",
      charlson ~ "Índice de Charlson",
      karnofsky ~ "Karnofsky",  # TODO: ¿mostrar entero o agruparlo?
      va ~ "Acceso Vascular"),
    statistic = list(
      age ~ "{mean} (\u00B1{sd}) años"),
    value = list(
      gender ~ "Mujer",
      c(htn, diabetes) ~ "Si"),
    missing = "no"
  ) |> 
  add_p(test.args = c(karnofsky) ~ list(simulate.p.value = TRUE)) |> 
  modify_header(label = "**Variable**",
                p.value = "**_p_**")

# Muestra la tabla comparativa demográfica
print(demogr_tbl)


# Prepare analysis dataset with all covariates ####
analysis_df <- main_df |> 
  # Select relevant columns
  select(id, gender, htn, diabetes, contains("hosp")) |> 
  
  # Convert to long format for periods
  pivot_longer(
    cols = matches("hosp.*_[1-9]"),
    names_to = c(".value", "adm_number"),
    names_pattern = "(.+?)_([1-9])",
    values_drop_na = TRUE
  ) |> 
  
  # Categorize hospitalizations by flood period (y traducir hospitalizaciones)
  mutate(
    period = factor(case_when(
      hosp_start > period_predana_start & hosp_end < period_predana_end ~ "Pre-DANA",
      hosp_start > period_postdana_start & hosp_end < period_postdana_end ~ "Post-DANA",
      TRUE ~ NA_character_),
      levels = c("Pre-DANA", "Post-DANA")),
    hosp_cause = case_match(
      hosp_cause,
      "Infection Respiratory" ~ "Infección Respiratoria",
      "Cardiovascular Pulmonary Disease" ~ "Enfermedad Pulmonar Cardiovascular",
      "Digestive System Disorder" ~ "Trastorno del Sistema Digestivo",
      "Other" ~ "Otras Causas",
      "Access Repair" ~ "Reparación de Acceso Vascular",
      "Musculoskeletal System Disorder" ~ "Trastorno del Sistema Musculoesquelético",
      "Infection Blood" ~ "Bacteriemia",
      "Cardiovascular Vessel Disease" ~ "Enfermedad Cardiovascular Periférica",
      "Genitourinary Disorder" ~ "Trastorno Genitourinario",
      "Cardiovascular Heart Disease" ~ "Enfermedad Cardiovascular",
      "Access Infection" ~ "Infección del Acceso Vascular",
      "Immune Disorder Other" ~ "Otros Trastornos Inmunológicos",
      "Unknown" ~ "Desconocido",
      "Infection Other" ~ "Otras Infecciones",
      "Nervous System Disorder" ~ "Trastorno del Sistema Nervioso",
      "Access Other" ~ "Otras Causas - Acceso Vascular",
      "Circulatory Disease" ~ "Enfermedad Circulatoria",
      "Infection Wound" ~ "Infección de la Herida",
      "Respiratory System Other Cause" ~ "Otras Causas Respiratorias",
      "Infection COVID19 (SARS-CoV2 infection)" ~ "Infección COVID19 (Infección por SARS-CoV2)",
      "Access Placement" ~ "Colocación de Acceso Vascular",
      "Blood Disorder Anemia" ~ "Anemia",
      "Cerebrovascular Disease" ~ "Enfermedad Cerebrovascular",
      "Injury/Accident" ~ "Lesión/Accidente",
      "Cardiovascular Other" ~ "Otros Trastornos Cardiovasculares",
      "Blood Disorder Clotting" ~ "Trastorno de la Coagulación",
      "Blood Disorder Other" ~ "Otros Trastornos Hematológicos",
      "Endocrine Disorder Diabetes" ~ "Diabetes Mellitus",
      "Cancer/Neoplasms" ~ "Cáncer/Neoplasmas",
      "Metabolic Disorder" ~ "Trastorno Metabólico",
      "Respiratory System COPD" ~ "Enfermedad Pulmonar Obstructiva Crónica (EPOC)",
      "Mental Disorder" ~ "Trastorno Mental",
      "Infection Peritonitis (PD Related Peritonitis Excluded)" ~ "Peritonitis (Excluyendo Peritonitis Relacionada con PD)",
      "Skin Disorder" ~ "Trastorno de la Piel",
      "Infection Tunnel/Cuff" ~ "Infección de Túnel/Cuff",
      "PD Catheter Replacement" ~ "Reemplazo de Catéter PD",
      "Other PD Related Cause" ~ "Otras Causas Relacionadas con PD",
      "PD Catheter Placement" ~ "Colocación de Catéter PD"
    ),
    hosp_cause_groups = factor(case_match(
      hosp_cause,
      "Infección Respiratoria" ~ "Infección",
      "Enfermedad Pulmonar Cardiovascular" ~ "Cardiovascular",
      "Trastorno del Sistema Digestivo" ~ "Otras Causas",
      "Otras Causas" ~ "Otras Causas",
      "Reparación de Acceso Vascular" ~ "Acceso Vascular (excl. infección)",
      "Trastorno del Sistema Musculoesquelético" ~ "Otras Causas",
      "Bacteriemia" ~ "Infección",
      "Enfermedad Cardiovascular Periférica" ~ "Cardiovascular",
      "Trastorno Genitourinario" ~ "Otras Causas",
      "Enfermedad Cardiovascular" ~ "Cardiovascular",
      "Infección del Acceso Vascular" ~ "Infección",
      "Otros Trastornos Inmunológicos" ~ "Otras Causas",
      "Desconocido" ~ "Otras Causas",
      "Otras Infecciones" ~ "Infección",
      "Trastorno del Sistema Nervioso" ~ "Otras Causas",
      "Otras Causas - Acceso Vascular" ~ "Acceso Vascular (excl. infección)",
      "Enfermedad Circulatoria" ~ "Cardiovascular",
      "Infección de la Herida" ~ "Infección",
      "Otras Causas Respiratorias" ~ "Otras Causas",
      "Infección COVID19 (Infección por SARS-CoV2)" ~ "Infección",
      "Colocación de Acceso Vascular" ~ "Acceso Vascular (excl. infección)",
      "Anemia" ~ "Otras Causas",
      "Enfermedad Cerebrovascular" ~ "Cardiovascular",
      "Lesión/Accidente" ~ "Otras Causas",
      "Otros Trastornos Cardiovasculares" ~ "Cardiovascular",
      "Trastorno de la Coagulación" ~ "Otras Causas",
      "Otros Trastornos Hematológicos" ~ "Otras Causas",
      "Diabetes Mellitus" ~ "Otras Causas",
      "Cáncer/Neoplasmas" ~ "Otras Causas",
      "Trastorno Metabólico" ~ "Otras Causas",
      "Enfermedad Pulmonar Obstructiva Crónica (EPOC)" ~ "Otras Causas",
      "Trastorno Mental" ~ "Otras Causas",
      "Peritonitis (Excluyendo Peritonitis Relacionada con PD)" ~ "Infección",
      "Trastorno de la Piel" ~ "Otras Causas",
      "Infección de Túnel/Cuff" ~ "Infección",
      "Reemplazo de Catéter PD" ~ "Otras Causas",
      "Otras Causas Relacionadas con PD" ~ "Otras Causas",
      "Colocación de Catéter PD" ~ "Otras Causas"),
      levels = c("Cardiovascular", "Infección", 
                 "Acceso Vascular (excl. infección)", "Otras Causas")),
    person_time = time_length(interval(hosp_start, hosp_end), unit = "days"),
    log_person_time = if_else(person_time <= 0, log(0.1), log(person_time)) # Handle zero/negative
  ) |> 
  filter(!is.na(period)) |> 
  
  # Add patient characteristics
  right_join(
    distinct(select(demogr_df, id, gender, age, htn, diabetes, charlson, va, period)),
    by = c("id", "period", "gender", "htn", "diabetes")
  ) |> 
  
  # Create analysis variables
  mutate(
    age_group = cut(age, breaks = c(18, 50, 65, 75, Inf), 
                    labels = c("Adulto", "Adulto mayor", "Anciano", "Anciano mayor"),
                    # TODO: comparar si hay diferencias en los resultados según sea ordinal o no
                    # ordered_result = TRUE
                    ))

# 1. Overall Hospitalization percentage by Period and Cause
tbl_overall <- analysis_df |> 
  tbl_strata(
    strata = period,
    .tbl_fun =
      ~ .x |>
      tbl_summary(
        include = c(hosp_cause_groups, age_group, gender, htn, diabetes, charlson),
        by = hosp_cause_groups,
        statistic = charlson ~ "{median} [{p25}, {p75}]",
        label = list(
          age_group ~ "Grupo Etario",
          gender ~ "Género",
          htn ~ "Hipertensión Arterial",
          diabetes ~ "Diabetes Mellitus",
          charlson ~ "Índice de Charlson"),
        value = c("htn", "diabetes") ~ "Si",
        missing = "no"
      ) |>
      add_p(test.args = age_group ~ list(simulate.p.value = TRUE)),
    .header = "**{strata}**"
  )

# Visualizar
print(tbl_overall)

# 2. Risk Ratio Analysis Using Negative Binomial Regression

model_data <- analysis_df |> 
  group_by(id, period) |> 
  mutate(hosp_count = n()) |> 
  ungroup() |> 
  distinct(id, period, adm_number, .keep_all = TRUE) |> 
  filter(
    !is.na(hosp_count),
    !is.infinite(hosp_count)
  )

model <- glm.nb(
  hosp_count ~ period + hosp_cause_groups + age_group + gender + htn + diabetes + charlson +
    offset(log_person_time),
  data = model_data,
  control = glm.control(maxit = 100) # Increase iterations
)

# Format results with gtsummary
tbl_model <- tbl_regression(
  model,
  exponentiate = TRUE,
  label = list(
    period ~ "Periodo",
    hosp_cause_groups ~ "Causas de Hospitalization",
    age_group ~ "Grupo Etario",
    charlson ~ "Índice de Charlson")) |> 
  add_global_p() |> 
  modify_header(label ~ "**Variable**")

print(tbl_model)

# 4. Visualization of Key Results
ggplot(analysis_df |> 
         count(period, hosp_cause_groups, age_group), 
       aes(x = hosp_cause_groups, y = n, fill = period)) +
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
  Risk_Factors = tbl_model
)
