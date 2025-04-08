# Cargar paquetes en memoria ####
require(tidyverse)
require(lubridate)
require(gtsummary)

# Definir locale, cargar dataframe y renombrar columnas ####

locale_es <- locale(date_names = "es", 
                    decimal_mark = ",",
                    grouping_mark = ".")
 
main_df <- read_csv("./data/raw/dana_24_25.csv",
                    col_names = 
                      c("id", "status_date_p1", "clinic_p1", "status_p1", 
                        "substatus_p1", "status_date_p2", "clinic_p2", "status_p2", 
                        "substatus_p2", "birth_date", "gender", "charlson_p1", "charlson_p2", 
                        "karnofsky_p1", "karnofsky_p2", "diabetes_type", "diabetes_date", 
                        "va_p1", "va_p2", "htn", "death_date", "death_cause", 
                        "hosp_start_1", "hosp_end_1", "hosp_re_1", "hosp_cause_1",
                        "hosp_start_2", "hosp_end_2", "hosp_re_2", "hosp_cause_2",
                        "hosp_start_3", "hosp_end_3", "hosp_re_3", "hosp_cause_3",
                        "hosp_start_4", "hosp_end_4", "hosp_re_4", "hosp_cause_4",
                        "hosp_start_5", "hosp_end_5", "hosp_re_5", "hosp_cause_5",
                        "hosp_start_6", "hosp_end_6", "hosp_re_6", "hosp_cause_6",
                        "hosp_start_7", "hosp_end_7", "hosp_re_7", "hosp_cause_7",
                        "hosp_start_8", "hosp_end_8", "hosp_re_8", "hosp_cause_8",
                        "hosp_start_9", "hosp_end_9", "hosp_re_9", "hosp_cause_9"),
                    col_types = "cccccccccccddddccccccccccccccccccccccccccccccccccccccccccc",
                    locale = locale_es,
                    skip = 1) |> 
  
  # Definir las varibles tipo "fecha" y "factor"
  mutate(
    across(contains("date"), dmy),
    across(contains("start"), dmy),
    across(contains("_end_"), dmy),
    across(contains("hosp_re"), ~ factor(.x,
                                         levels = c("No", "Yes"),
                                         labels = c("No", "Si"))),
    gender = factor(gender,
                    levels = c("F", "M"),
                    labels = c("Mujer", "Hombre")),
    htn = factor(htn,
                 levels = c("N", "Y"),
                 labels = c("No", "Si")),
    diabetes = factor(!is.na(diabetes_type),
                      levels = c("FALSE", "TRUE"),
                      labels = c("No", "Si")))


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
  mutate(period = case_when(                       # están presentes un periodo y aquellos que iniciaron después del periodo
    period == "p1" ~ "2023",
    period == "p2" ~ "2024"),
    age = as.numeric(difftime(status_date, birth_date, units = "days")) / 365.25)

# Código para generar la tabla comparativa demográfica
demogr_tb <- demogr_df |> 
  tbl_summary(
    by = period,
    include = c(age, gender, htn, diabetes, status, charlson, karnofsky, va),
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
  add_p(test.args = c(karnofsky, status) ~ list(simulate.p.value = TRUE))

print(demogr_tb)


# Generar dataframe para extraer análisis de hospitalizaciones ####
# Primero convertir a formato largo, para generar nueva columna con la información de los periodos

hosp_df <- main_df |> 
  select(id, contains("hosp")) |> 
  pivot_longer(
    cols = matches("_[1-9]"),
    names_to = c(".value", "period"),
    names_pattern = "(.+?)_([1-9])") |> 
  filter(!is.na(hosp_start)) |>   # Recoge todos los eventos hospitalarios en ambos periodos
  mutate(
    hosp_length = time_length(interval(hosp_start, hosp_end), "days"),
    hosp_cause_groups = factor(case_match(
      hosp_cause,
      "Infection Respiratory" ~ "Infección",
      "Cardiovascular Pulmonary Disease" ~ "Cardiovascular",
      "Digestive System Disorder" ~ "Otras Causas",
      "Other" ~ "Otras Causas",
      "Access Repair" ~ "Acceso Vascular (no infección)",
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
      "Access Other" ~ "Acceso Vascular (no infección)",
      "Circulatory Disease" ~ "Cardiovascular",
      "Infection Wound" ~ "Infección",
      "Respiratory System Other Cause" ~ "Otras Causas",
      "Infection COVID19 (SARS-CoV2 infection)" ~ "Infección",
      "Access Placement" ~ "Acceso Vascular (no infección)",
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
      "PD Catheter Placement" ~ "Colocación de Catéter PD"))


# Ánalisis de hospitalizaciones ####


