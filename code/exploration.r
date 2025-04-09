# Carga los paquetes a usar, junto con la versión limpia del dataframe ####
# Este es el documento de experimentación inicial 

source("./code/cleaning.r")

# Generar dataframe para extraer análisis demográfico ####
# Primero convertir a formato largo, para generar nueva columna con la información del periodo
# y poder usar ''gtsummary'' para la tabla comparativa de poblaciones
demogr_df <- main_df |> 
  select(id, gender, birth_date, htn, diabetes, diabetes_type, contains("p1"), contains("p2")) |> 
  pivot_longer(
    cols = matches("_p[12]"),
    names_to = c(".value", "period"),
    names_pattern = "(.+?)_(p[12])") |> 
  filter(!is.na(clinic) & !is.na(va)) |>           # Este filter permite eliminar entradas duplicadas de pacientes que solo
  mutate(period = factor(case_when(                       # están presentes un periodo y aquellos que iniciaron después del periodo
    period == "p1" ~ "Pre-DANA",
    period == "p2" ~ "Post-DANA"),
    levels = c("Pre-DANA", "Post-DANA")),
    age = if_else(
      period == "Pre-DANA",
      time_length(interval(birth_date, period_predana_start), "years"),
      time_length(interval(birth_date, period_postdana_start), "years")))

# Código para generar la tabla comparativa demográfica
demogr_tbl <- demogr_df |> 
  tbl_summary(
    by = period,
    include = c(age, gender, htn, diabetes, charlson, karnofsky, va),
    label = list(
      age ~ "Edad",
      gender ~ "Mujeres",
      htn ~ "Hipertensión",
      diabetes ~ "Diabetes", 
      charlson ~ "Índice de Charlson",
      karnofsky ~ "Karnofsky",  # TODO: ¿mostrar entero o agruparlo?
      va ~ "Acceso Vascular"),
    statistic = list(
      age ~ "{mean} (\u00B1{sd})"),
    value = list(
      gender ~ "Mujer",
      c(htn, diabetes) ~ "Si")
  ) |> 
  add_p(test.args = c(karnofsky) ~ list(simulate.p.value = TRUE))

print(demogr_tbl)


# Generar dataframe para extraer análisis de hospitalizaciones ####
# Primero convertir a formato largo, para generar nueva columna con la información de los periodos

hosp_df <- main_df |> 
  select(id, contains("hosp")) |> 
  pivot_longer(
    cols = matches("_[1-9]"),
    names_to = c(".value", "period"),
    names_pattern = "(.+?)_([1-9])") |> 
  filter(!is.na(hosp_start)) |>   # Recoge solo pacientes con eventos hospitalarios en ambos periodos
  mutate(
    period = case_when(
      (hosp_start > period_predana_start) & (hosp_end < period_predana_end) ~ "Pre-Dana",
      (hosp_start > period_postdana_start) & (hosp_end < period_postdana_end) ~ "Post-Dana"),
    hosp_cause_groups = factor(case_match(
      hosp_cause,
      "Infection Respiratory" ~ "Infección",
      "Cardiovascular Pulmonary Disease" ~ "Cardiovascular",
      "Digestive System Disorder" ~ "Otras Causas",
      "Other" ~ "Otras Causas",
      "Access Repair" ~ "Acceso Vascular (excl. infección)",
      "Musculoskeletal System Disorder" ~ "Otras Causas",
      "Infection Blood" ~ "Infección",
      "Cardiovascular Vessel Disease" ~ "Cardiovascular",
      "Genitourinary Disorder" ~ "Urológicas",
      "Cardiovascular Heart Disease" ~ "Cardiovascular",
      "Access Infection" ~ "Infección",
      "Immune Disorder Other" ~ "Otras Causas",
      "Unknown" ~ "Desconocido",
      "Infection Other" ~ "Infección",
      "Nervous System Disorder" ~ "Otras Causas",
      "Access Other" ~ "Acceso Vascular (excl. infección)",
      "Circulatory Disease" ~ "Cardiovascular",
      "Infection Wound" ~ "Infección",
      "Respiratory System Other Cause" ~ "Otras Causas",
      "Infection COVID19 (SARS-CoV2 infection)" ~ "Infección",
      "Access Placement" ~ "Acceso Vascular (excl. infección)",
      "Blood Disorder Anemia" ~ "Otras Causas",
      "Cerebrovascular Disease" ~ "Cardiovascular",
      "Injury/Accident" ~ "Otras Causas",
      "Cardiovascular Other" ~ "Cardiovascular",
      "Blood Disorder Clotting" ~ "Otras Causas",
      "Blood Disorder Other" ~ "Otras Causas",
      "Endocrine Disorder Diabetes" ~ "Otras Causas",
      "Cancer/Neoplasms" ~ "Otras Causas",
      "Metabolic Disorder" ~ "Otras Causas",
      "Respiratory System COPD" ~ "Otras Causas",
      "Mental Disorder" ~ "Otras Causas",
      "Infection Peritonitis (PD Related Peritonitis Excluded)" ~ "Infección",
      "Skin Disorder" ~ "Otras Causas",
      "Infection Tunnel/Cuff" ~ "Infección",
      "PD Catheter Replacement" ~ "Otras Causas",
      "Other PD Related Cause" ~ "Otras Causas",
      "PD Catheter Placement" ~ "Otras Causas")),
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
      "Access Infection" ~ "Infección de Acceso Vascular",
      "Immune Disorder Other" ~ "Otros Trastornos Inmunológicos",
      "Unknown" ~ "Desconocido",
      "Infection Other" ~ "Otras Infecciones",
      "Nervous System Disorder" ~ "Trastorno del Sistema Nervioso",
      "Access Other" ~ "Otras Causas - Acceso Vascular",
      "Circulatory Disease" ~ "Enfermedad Circulatoria",
      "Infection Wound" ~ "Infección de Herida",
      "Respiratory System Other Cause" ~ "Otras Causas Respiratorias",
      "Infection COVID19 (SARS-CoV2 infection)" ~ "Infección COVID19 (Infección por SARS-CoV2)",
      "Access Placement" ~ "Colocación de Acceso Vascular",
      "Blood Disorder Anemia" ~ "Anemia",
      "Cerebrovascular Disease" ~ "Enfermedad Cerebrovascular",
      "Injury/Accident" ~ "Lesión/Accidente",
      "Cardiovascular Other" ~ "Otros Trastornos Cardiovasculares",
      "Blood Disorder Clotting" ~ "Trastorno de la Coagulación",
      "Blood Disorder Other" ~ "Otros Trastornos Hematológicos",
      "Endocrine Disorder Diabetes" ~ "Diabetes",
      "Cancer/Neoplasms" ~ "Cáncer/Neoplasmas",
      "Metabolic Disorder" ~ "Trastorno Metabólico",
      "Respiratory System COPD" ~ "Enfermedad Pulmonar Obstructiva Crónica (EPOC)",
      "Mental Disorder" ~ "Trastorno Mental",
      "Infection Peritonitis (PD Related Peritonitis Excluded)" ~ "Peritonitis (Excluyendo Peritonitis Relacionada con PD)",
      "Skin Disorder" ~ "Trastorno de la Piel",
      "Infection Tunnel/Cuff" ~ "Infección de Túnel/Cuff",
      "PD Catheter Replacement" ~ "Reemplazo de Catéter PD",
      "Other PD Related Cause" ~ "Otras Causas Relacionadas con PD",
      "PD Catheter Placement" ~ "Colocación de Catéter PD"),
    hosp_length = time_length(interval(hosp_start, hosp_end), "days")) |> 
  filter(!is.na(period)) |>       # Filtrar hospitalizaciones que ocurren fuera de los periodos de análisis
  mutate()


# Ánalisis de hospitalizaciones ####
hosp_summary <- hosp_df |> 
  group_by(period, hosp_cause_groups) |> 
  summarise(
    hosp_count = n(),
    hosp_length = sum(hosp_length),
    hosp_readmission = sum(hosp_re == "Si"))


hosp_tbl <- hosp_df |> 
  tbl_summary(
    by = hosp_cause_groups,
    include = c(period, hosp_length, hosp_re),
    label = list(
      period ~ "Periodo",
      hosp_length ~ "Días de hospitalización",
      hosp_re ~ "Reingreso"),
    statistic = list(
      all_continuous() ~ "{mean} (\u00B1{sd})"),
    value = hosp_re ~ "Si") |> 
  add_p(test.args = c(period) ~ list(simulate.p.value = TRUE))

print(hosp_tbl)

# Generar análisis de ratios de hospitalización por pacientes totales
hosp_ratio_df <- main_df |> 
  select(id, contains("hosp")) |> 
  pivot_longer(
    cols = matches("_[1-9]"),
    names_to = c(".value", "adm_number"),
    names_pattern = "(.+?)_([1-9])") |> 
  mutate(
    period = factor(case_when(
      (hosp_start > period_predana_start) & (hosp_end < period_predana_end) ~ "Pre-DANA",
      (hosp_start > period_postdana_start) & (hosp_end < period_postdana_end) ~ "Post-DANA"),
      levels = c("Pre-DANA", "Post-DANA")))|> 
  right_join(distinct(select(demogr_df, id, period)), by = c("id", "period")) |> 
  mutate(adm_number = if_else(is.na(adm_number), 0L, as.numeric(adm_number)),
         hosp_cause = factor(case_match(
           hosp_cause,
           "Infection Respiratory" ~ "Infección",
           "Cardiovascular Pulmonary Disease" ~ "Cardiovascular",
           "Digestive System Disorder" ~ "Otras Causas",
           "Other" ~ "Otras Causas",
           "Access Repair" ~ "Acceso Vascular (excl. infección)",
           "Musculoskeletal System Disorder" ~ "Otras Causas",
           "Infection Blood" ~ "Infección",
           "Cardiovascular Vessel Disease" ~ "Cardiovascular",
           "Genitourinary Disorder" ~ "Urológicas",
           "Cardiovascular Heart Disease" ~ "Cardiovascular",
           "Access Infection" ~ "Infección",
           "Immune Disorder Other" ~ "Otras Causas",
           "Unknown" ~ "Desconocido",
           "Infection Other" ~ "Infección",
           "Nervous System Disorder" ~ "Otras Causas",
           "Access Other" ~ "Acceso Vascular (excl. infección)",
           "Circulatory Disease" ~ "Cardiovascular",
           "Infection Wound" ~ "Infección",
           "Respiratory System Other Cause" ~ "Otras Causas",
           "Infection COVID19 (SARS-CoV2 infection)" ~ "Infección",
           "Access Placement" ~ "Acceso Vascular (excl. infección)",
           "Blood Disorder Anemia" ~ "Otras Causas",
           "Cerebrovascular Disease" ~ "Cardiovascular",
           "Injury/Accident" ~ "Otras Causas",
           "Cardiovascular Other" ~ "Cardiovascular",
           "Blood Disorder Clotting" ~ "Otras Causas",
           "Blood Disorder Other" ~ "Otras Causas",
           "Endocrine Disorder Diabetes" ~ "Otras Causas",
           "Cancer/Neoplasms" ~ "Otras Causas",
           "Metabolic Disorder" ~ "Otras Causas",
           "Respiratory System COPD" ~ "Otras Causas",
           "Mental Disorder" ~ "Otras Causas",
           "Infection Peritonitis (PD Related Peritonitis Excluded)" ~ "Infección",
           "Skin Disorder" ~ "Otras Causas",
           "Infection Tunnel/Cuff" ~ "Infección",
           "PD Catheter Replacement" ~ "Otras Causas",
           "Other PD Related Cause" ~ "Otras Causas",
           "PD Catheter Placement" ~ "Otras Causas"),
           levels = c("Cardiovascular", "Infección", "Acceso Vascular (excl. infección)", "Urológicas",
                      "Otras Causas", "Desconocido")))

# Calculate hospitalization ratios per period and cause
period_ratios <- hosp_ratio_df |> 
  
  # Count hospitalizations per period and cause
  group_by(period, hosp_cause) |> 
  summarise(hosp_count = n(), .groups = "drop") |> 
  
  # Calculate unique patients per period
  left_join(
    hosp_ratio_df |> 
      group_by(period) |> 
      summarise(unique_patients = n_distinct(id)),
    by = "period"
  ) |> 
  
  # Calculate ratio and format
  mutate(
    ratio = hosp_count / unique_patients,
    ratio_label = scales::percent(ratio, accuracy = 0.1)
  ) |> 
  
  # Complete missing cause-period combinations
  complete(period, hosp_cause, fill = list(
    hosp_count = 0, 
    ratio = 0, 
    ratio_label = "0.0%"
  )) |> 
  
  # Retira la ausencia de hospitalización como "Causa"
  filter(!is.na(hosp_cause)) |> 
  
  # Present results clearly
  select(Period = period, Cause = hosp_cause, 
         Hospitalizations = hosp_count, 
         Ratio = ratio, `Ratio (%)` = ratio_label)

# Visualiza
ggplot(period_ratios, aes(x = Cause, y = Ratio, fill = Period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Hospitalization Ratios by Period and Cause",
       y = "Hospitalization Ratio",
       x = "Cause of Hospitalization") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))