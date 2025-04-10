# Carga los paquetes a usar, junto con la versión limpia del dataframe ####
# Este es el documento de análisis final 

source("./code/cleaning.r")

# Prototipo inicial del código provisto por Perplexity (comentarios en inglés)

# Crear tabla para comparar datos demográficos y comorbilidades ####
demogr_df <- main_df |> 
  select(id, gender, birth_date, htn, diabetes, diabetes_type, contains("p1"), contains("p2")) |> 
  pivot_longer(cols = matches("_p[12]"),
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
      time_length(interval(birth_date, period_postdana_start), "years")),
    age_group = cut(age, breaks = c(18, 50, 65, 75, Inf), 
                    labels = c("18-49", "50-64", "65-74", "\u2265 75"),
                    # TODO: comparar si hay diferencias en los resultados según sea ordinal o no
                    # ordered_result = TRUE
    ))

# Genera tabla comparativa demográfica
demogr_tbl <- demogr_df |> 
  tbl_summary(
    by = period,
    include = c(age, age_group, gender, htn, diabetes, diabetes_type, charlson, karnofsky, va),
    label = list(
      age ~ "Edad",
      age_group ~ "Edad (Categórica)",
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
    distinct(select(demogr_df, id, clinic, gender, age, age_group, htn, diabetes, charlson, va, period)),
    by = c("id", "period", "gender", "htn", "diabetes"))
  

# 1. Overall Hospitalization percentage by Period and Cause ####
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

# 2. Análisis de hospitalizaciones apareado (solo pacientes presentes en ambos periodos) ####
# 2.1 Identificar pacientes en ambos periodos

patients_in_both_periods <- analysis_df |>
  group_by(id) |>
  summarize(unique_periods = n_distinct(period)) |>
  filter(unique_periods == 2) |>
  pull(id)

# 2.2 Dataset de pacientes apareados

# Crea un dataframe con todas las combinaciones de id * periodo y datos clínicos
all_patient_periods <- expand_grid(
  id = patients_in_both_periods,
  period = c("Pre-DANA", "Post-DANA")
) |> 
  # Añade datos demográficos
  left_join(
    demogr_df |> 
      select(id, period, gender, age, age_group, htn, diabetes, charlson, va, clinic) |> 
      distinct(id, period, .keep_all = TRUE),
    by = join_by(id, period)
  )

# Combinarlo con los eventos de hospitalización, si los hubiera, para crear el df de datos apareados.
paired_df <- all_patient_periods |>
  # LEFT join, para asegurarnos de que solo añadimos información a este df y no perdemos ninguna entrada
  left_join(
    analysis_df |> 
      filter(!is.na(hosp_cause_groups)) |>  # Extraigamos solo eventos de hospitalización
      group_by(id, period) |> 
      summarise(hosp_count = n(),
                infection_any = sum(hosp_cause_groups == "Infección") > 0,
                infection_count = sum(hosp_cause_groups == "Infección"),
                cardio_any = sum (hosp_cause_groups == "Cardiovascular") > 0,
                cardio_count = sum(hosp_cause_groups == "Cardiovascular"),
                .groups = "drop"),
    by = join_by(id, period)
  ) |> 
  # Finalmente, completamos los NA con información de los no hospitalizados
  mutate(
    hosp_any = if_else(is.na(hosp_count), 0, 1),
    across(c(contains("infection"), contains("cardio"), hosp_count), 
           ~ if_else(is.na(.x), 0, .x))
  )

# 2.3 Convertir en dataframe ancho para realizar los test de comparación
paired_wide <- paired_df |>
  pivot_wider(
    id_cols = c(id, gender, htn, diabetes),
    names_from = period,
    values_from = c(charlson, age_group, hosp_any, hosp_count, infection_any, infection_count, cardio_any, cardio_count),
    names_sep = "_"
  )

# 2.4 Pruebas estadísticas
paired_wide |> 
  mutate(
    hosp_diff = `hosp_count_Post-DANA` - `hosp_count_Pre-DANA`,
    infection_diff = `infection_count_Post-DANA` - `infection_count_Pre-DANA`,
    cardio_diff = `cardio_count_Post-DANA` - `cardio_count_Pre-DANA`
  ) |>
  summarize(
    mean_hosp_diff = mean(hosp_diff, na.rm = TRUE),
    p_hosp = t.test(`hosp_count_Post-DANA`, `hosp_count_Pre-DANA`, paired = TRUE)$p.value,
    mean_infection_diff = mean(infection_diff, na.rm = TRUE),
    p_infection = t.test(`infection_count_Post-DANA`, `infection_count_Pre-DANA`, paired = TRUE)$p.value,
    mean_cardio_diff = mean(cardio_diff, na.rm = TRUE),
    p_cardio = t.test(`cardio_count_Post-DANA`, `cardio_count_Pre-DANA`, paired = TRUE)$p.value
  )  

# Modelo de McNemar para todos, cardio e infección

mcnemar_overall <- with(paired_wide, 
                          table(factor(`hosp_any_Pre-DANA`, levels = c(0, 1)),
                                factor(`hosp_any_Post-DANA`, levels = c(0, 1))))
mcnemar.test(mcnemar_overall)


mcnemar_infection <- with(paired_wide, 
                          table(factor(`infection_any_Pre-DANA`, levels = c(0, 1)),
                                factor(`infection_any_Post-DANA`, levels = c(0, 1))))
mcnemar.test(mcnemar_infection)


mcnemar_cardio <- with(paired_wide, 
                          table(factor(`cardio_any_Pre-DANA`, levels = c(0, 1)),
                                factor(`cardio_any_Post-DANA`, levels = c(0, 1))))
mcnemar.test(mcnemar_cardio)

# Determinar la fuerza del efecto (spoiler alert, es débil)
effsize::cohen.d(paired_wide$`hosp_count_Post-DANA`, 
                 paired_wide$`hosp_count_Pre-DANA`, paired=TRUE)



# 3. Análisis con datos no apareados ####

# Create complete set of actual patient-period combinations
observed_patient_periods <- analysis_df |>
  select(id, period) |>
  distinct()

# Create patient dataset with hospitalization flags
unpaired_data <- observed_patient_periods |>
  # Add demographic data
  left_join(
    main_df |> 
      select(id, gender, htn, diabetes, charlson, age_group) |> 
      distinct(id, .keep_all = TRUE),
    by = "id"
  ) |>
  # Add hospitalization data
  left_join(
    analysis_df |>
      group_by(id, period) |>
      summarize(
        was_hospitalized = 1,
        hosp_count = n(),
        any_infection = sum(hosp_cause_groups == "Infección") > 0,
        infection_count = sum(hosp_cause_groups == "Infección"),
        .groups = "drop"
      ),
    by = c("id", "period")
  ) |>
  # Fill NA values for non-hospitalized
  mutate(
    was_hospitalized = ifelse(is.na(was_hospitalized), 0, was_hospitalized),
    hosp_count = ifelse(is.na(hosp_count), 0, hosp_count),
    any_infection = ifelse(is.na(any_infection), 0, any_infection),
    infection_count = ifelse(is.na(infection_count), 0, infection_count),
    # Record whether patient was in both periods (for stratification)
    in_both_periods = id %in% patients_in_both_periods
  )

# Overall hospitalization risk model
hosp_risk_model <- glm(
  was_hospitalized ~ period + age_group + gender + htn + diabetes + charlson,
  family = binomial(link = "logit"),
  data = unpaired_data
)

# Infection-specific model
infection_risk_model <- glm(
  any_infection ~ period + age_group + gender + htn + diabetes + charlson,
  family = binomial(link = "logit"),
  data = unpaired_data
)

# Count models with exposure time
unpaired_data <- unpaired_data |>
  mutate(
    exposure_time = case_when(
      period == "Pre-DANA" ~ as.numeric(difftime(period_predana_end, period_predana_start, units="days")),
      period == "Post-DANA" ~ as.numeric(difftime(period_postdana_end, period_postdana_start, units="days"))
    ),
    log_exposure = log(exposure_time)
  )

hosp_count_model <- glm.nb(
  hosp_count ~ period + age_group + gender + htn + diabetes + charlson + 
    offset(log_exposure),
  data = unpaired_data
)
