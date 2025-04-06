# Cargar paquetes en memoria ####
require(tidyverse)
require(lubridate)

# Definir locale, cargar dataframe y renombrar columnas ####

locale_es <- locale(date_names = "es", 
                    decimal_mark = ",",
                    grouping_mark = ".")
# 
main_df <- read_csv("./data/raw/dana_24_25.csv",
                    col_names = 
                      c("id", "status_date_p1", "clinic_p1", "status_p1", 
                        "substatus_p1", "status_date_p2", "clinic_p2", "status_p2", 
                        "substatus_p2", "bith_date", "gender", "charlson_p1", "charlson_p2", 
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
  mutate(
    across(contains("date"), dmy),
    across(contains("start"), dmy),
    across(contains("end"), dmy),
    across(contains("hosp_re"), factor),
    gender = factor(gender),
    htn = factor(htn),
    diabetes = factor(!is.na(diabetes_type))
    )

    