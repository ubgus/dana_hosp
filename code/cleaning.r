# Cargar paquetes en memoria ####
require(tidyverse)
require(lubridate)
require(gtsummary)
require(ggplot2)

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
  
  # Definir las variables tipo "fecha" y "factor"
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


# Definir los periodos de análisis

period_predana_start <- date("2023-10-28")
period_predana_end <- date("2024-02-28")

period_postdana_start <- date("2024-10-28")
period_postdana_end <- date("2025-02-28")

