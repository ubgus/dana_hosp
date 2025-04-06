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
  filter(!is.na(clinic)) |>           # Este filter permite eliminar entradas duplicadas de pacientes 
  mutate(period = case_when(          # que solo están presentes un periodo
    period == "p1" ~ "2023",
    period == "p2" ~ "2024"
  ))

# Codigo para generar la tabla comparativa demográfica
demogr_tb <- demogr_df |> 
  tbl_summary(
    by = period,
    include = c(gender, htn, diabetes, status, charlson, karnofsky, va),
    label = list( # TODO: traducir las etiquetas al español
      gender ~ "Gender",
      htn ~ "Hypertension",
      diabetes ~ "Diabetes", 
      status ~ "Clinical Status",  # TODO: definir si vamos a utilizar esta variable y como
      charlson ~ "Charlson Comorbidity Index",
      karnofsky ~ "Karnofsky Performance Score",
      va ~ "Vascular Access"),  # TODO: definir que hacer con los pacientes que inician después del inicio del periodo
    value = list(
      gender ~ "Mujer",
      c(htn, diabetes) ~ "Si")
  ) |> 
  add_p() # TODO: añadir 'simulate.p.value=TRUE' en esta sección para eliminar los errores de calculo de p

print(demogr_tb)

