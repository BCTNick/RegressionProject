## @knitr librerie ----------------------------------------------------------------

library(eurostat)
library(tidyverse)
library(readr)
library(ggplot2)
library(tmap)
library(sf)
library(kableExtra)
library(GGally)
library(plotly)
library(tidycensus)
library(corrr)
library(spdep)
library(tigris)
library(rmapshaper)
library(flextable)
library(spatialreg)
library(stargazer)
library(car)
library(MASS)
library(tseries)
library(nortest)
library(leaps)
library(lmtest)
library(gap)
library(dplyr)
library(strucchange)
library(lme4)
library(Matrix)
library(kableExtra)
library(caret) 
library(quantreg)
library(nlme)


# NUTS2 <- get_eurostat_geospatial(output_class = "df",
#                                  resolution = "60",
#                                  nuts_level = "2") |>
#   filter(CNTR_CODE != "AL") |>
#   filter(CNTR_CODE != "CH") |>
#   filter(CNTR_CODE != "IS") |>
#   filter(CNTR_CODE != "LI") |>
#   filter(CNTR_CODE != "ME") |>
#   filter(CNTR_CODE != "MK") |>
#   filter(CNTR_CODE != "NO") |>
#   filter(CNTR_CODE != "RS") |>
#   filter(CNTR_CODE != "TR") |>
#   filter(CNTR_CODE != "UK")
# 
# 
# # Y -----------------------------------------------------------------------
# 
# esl <-
#   get_eurostat_json("edat_lfse_16",
#                     filters = list(
#                       geo = NUTS2$id,
#                       age = "Y18-24",
#                       sex = "T",
#                       time = 2023
#                     ))
# 
# for (anno in 2022:2018) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(esl$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(esl$values))
# 
#     # Scarica i dati per l'anno precedente
#     esl_previous <-
#       get_eurostat_json("edat_lfse_16",
#                         filters = list(
#                           geo = NUTS2$id,
#                           age = "Y18-24",
#                           sex = "T",
#                           time = anno
#                         ))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- esl$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         esl_previous$values[esl_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         esl$values[i] <- previous_value
#         esl$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(esl_previous)
# 
# # Mean disposable income of private households -----------------------------------------------------------------------
# 
# disp_inc <-
#   get_eurostat_json("tgs00026",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2022
#                     ))
# 
# 
# for (anno in 2021:2018) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(disp_inc$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(disp_inc$values))
# 
#     # Scarica i dati per l'anno precedente
#     disp_inc_previous <-
#       get_eurostat_json("tgs00026",
#                         filters = list(
#                           geo = NUTS2$id,
#                           time = anno
#                         ))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- disp_inc$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         disp_inc_previous$values[disp_inc_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         disp_inc$values[i] <- previous_value
#         disp_inc$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(disp_inc_previous)
# 
# 
# ##moltiplica per un milione e dividi per la popolazione
# pop <-
#   get_eurostat_json("demo_r_d2jan",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2022,
#                       sex = "T",
#                       age = "TOTAL"
#                     ))
# 
# 
# for (anno in 2021:2018) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(pop$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(pop$values))
# 
#     # Scarica i dati per l'anno precedente
#     pop_previous <-
#       get_eurostat_json("demo_r_d2jan",
#                         filters = list(
#                           geo = NUTS2$id,
#                           time = anno,
#                           sex = "T",
#                           age = "TOTAL"
#                         ))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- pop$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         pop_previous$values[pop_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         pop$values[i] <- previous_value
#         pop$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(pop_previous)
# 
# # Calcola il reddito disponibile pro capite (in migliaia di euro)
# disp_inc$values <- disp_inc$values / pop$values[match(disp_inc$geo, pop$geo)] * 1000
# 
# 
# # Density (100 ab. per km2) -----------------------------------------------------------------
# 
# density <-
#   get_eurostat_json("tgs00024",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2022
#                     ))
# 
# 
# for (anno in 2021:2018) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(density$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(density$values))
# 
#     # Scarica i dati per l'anno precedente
#     density_previous <-
#       get_eurostat_json("tgs00024",
#                         filters = list(
#                           geo = NUTS2$id,
#                           time = anno))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- density$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         density_previous$values[density_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         density$values[i] <- previous_value
#         density$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(density_previous)
# density$values <- density$values / 100
# 
# # Unemployment -----------------------------------------------------------------
# 
# unemployment <-
#   get_eurostat_json("tgs00010",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2023,
#                       sex = "T",
#                       isced11 ="TOTAL"
#                     ))
# 
# 
# for (anno in 2022:2019) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(unemployment$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(unemployment$values))
# 
#     # Scarica i dati per l'anno precedente
#     unemployment_previous <-
#       get_eurostat_json("tgs00010",
#                         filters = list(
#                           geo = NUTS2$id,
#                           time = anno,
#                           sex = "T",
#                           isced11 ="TOTAL"))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- unemployment$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         unemployment_previous$values[unemployment_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         unemployment$values[i] <- previous_value
#         unemployment$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(unemployment_previous)
# 
# 
# # HRST (% on total labor force)--------------------------------------------------------------------
# 
# HRST <-
#   get_eurostat_json("tgs00038",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2023
#                     ))
# 
# 
# for (anno in 2022:2019) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(HRST$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(HRST$values))
# 
#     # Scarica i dati per l'anno precedente
#     HRST_previous <-
#       get_eurostat_json("tgs00038",
#                         filters = list(
#                           geo = NUTS2$id,
#                           time = anno
#                           ))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- HRST$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         HRST_previous$values[HRST_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         HRST$values[i] <- previous_value
#         HRST$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(HRST_previous)
# 
# 
# # Over 25 with at most lower secondary education (%) -------------------------
# 
# amLSE <-
#   get_eurostat_json("edat_lfse_04",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2023,
#                       sex = "T",
#                       age = "Y25-64",
#                       isced11 = "ED0-2"
#                     ))
# 
# 
# for (anno in 2022:2019) {
#   # Verifica se ci sono valori mancanti
#   if (any(is.na(amLSE$values))) {
#     # Trova gli indici delle celle mancanti
#     na_indices <- which(is.na(amLSE$values))
# 
#     # Scarica i dati per l'anno precedente
#     amLSE_previous <-
#       get_eurostat_json("edat_lfse_04",
#                         filters = list(
#                           geo = NUTS2$id,
#                           time = anno,
#                           sex = "T",
#                           age = "Y25-64",
#                           isced11 = "ED0-2"
#                         ))
# 
#     # Imputa i dati mancanti
#     for (i in na_indices) {
#       geo_id <- amLSE$geo[i]
#       # Trova il valore corrispondente nell'anno precedente
#       previous_value <-
#         amLSE_previous$values[amLSE_previous$geo == geo_id]
#       # Imputa il valore se esiste
#       if (length(previous_value) > 0) {
#         amLSE$values[i] <- previous_value
#         amLSE$time[i] <- anno
#       }
#     }
#   }
# }
# 
# rm(amLSE_previous)
# 
# 
# # tourism -------------------------
# 
# tourism <-
#   get_eurostat_json("tgs00111",
#                     filters = list(
#                       geo = NUTS2$id,
#                       time = 2022,
#                       c_resid = "TOTAL"
#                     ))
# 
# 
# tourism$values <- tourism$values / pop$values[match(tourism$geo, pop$geo)]
# 
# tourism <- tourism[!is.na(tourism$values), ]
# 
# 
# european region  --------------------------------------------------------

region_mapping <- list(
  "Southern Europe" = c("AL", "CY", "EL", "ES", "IT", "MT", "PT"),
  "Western Europe" = c("AT", "BE", "CH", "FR", "DE", "LI", "LU", "NL"),
  "Northern Europe" = c("DK", "EE", "FI", "IE", "IS", "LT", "LV", "NO", "SE", "UK"),
  "Eastern Europe" = c("BG", "CZ", "HU", "PL", "RO", "SI", "SK", "HR", "RS", "ME", "MK", "TR")
)

assign_region <- function(nuts2_code) {
  country_code <- substr(nuts2_code, 1, 2)
  for (region in names(region_mapping)) {
    if (country_code %in% region_mapping[[region]]) {
      return(region)
    }
  }
  return(NA)
}


# # # gini coefficient inequality country ---------------------------------------------
# # 
# # gini_country <-
# #   get_eurostat_json("tessi190",
# #                     filters = list(
# #                       geo = NUTS2$CNTR_CODE,
# #                       time = 2023
# #                     ))
# # 
# # 
# # 
# # # migration ---------------------------------------------------------------
# # 
# # migration_country <-
# #   get_eurostat_json("migr_imm8",
# #                     filters = list(
# #                       geo = NUTS2$CNTR_CODE,
# #                       time = 2022,
# #                       sex = "T",
# #                       age = "TOTAL",
# #                       agedef = "COMPLET"
# #                     ))
# # 
# # 
# # ##percentuale sul totale della popolazione
# # 
# # pop_country <-
# #   get_eurostat_json("demo_r_d2jan",
# #                     filters = list(
# #                       geo = NUTS2$CNTR_CODE,
# #                       time = 2022,
# #                       sex = "T",
# #                       age = "TOTAL"
# #                     ))
# # migration_country$values <- migration_country$values / pop_country$values[match(migration_country$geo, pop_country$geo)] * 100
# 
# 
# # settimane di festa ------------------------------------------------------
# 
# holydays_gr_10w <- data.frame(geo = unique(NUTS2$CNTR_CODE)) |>
#   mutate(values = ifelse(geo %in% c("BG",
#                                     "CY",
#                                     "EL",
#                                     "IT",
#                                     "PT",
#                                     "ES",
#                                     "IE",
#                                     "EE",
#                                     "LV",
#                                     "MT",
#                                     "HR",
#                                     "HU",
#                                     "RO"), 1, 0))
# 
## @knitr dataset completo --------------------------------------------------------
# 
# dataset <- data.frame()
# dataset <- NUTS2[, c("id", "NAME_LATN")] |>
#   rename(nuts_id = id,
#          nuts_name = NAME_LATN) |>
#   left_join(esl [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(esl = values)|>
#   left_join(disp_inc [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(disp_inc = values) |>
#   left_join(density [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(density = values) |>
#   left_join(unemployment [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(unemployment = values)|>
#   left_join(HRST [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(HRST = values)|>
#   left_join(amLSE [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(amLSE = values)|>
#   left_join(tourism [, c("geo", "values")], by = c("nuts_id" = "geo"))  |>
#   rename(tourism = values)|>
#   left_join(NUTS2 [, c("geo", "CNTR_CODE")], by = c("nuts_id" = "geo"))  |>
#   rename(country = CNTR_CODE) |>
#   left_join(holydays_gr_10w [, c("geo", "values")], by = c("country" = "geo"))  |>
#   rename(holydays_gr_10w = values) |>
#   dplyr::select(-country)
# 
# dataset$holydays_gr_10w <- as.factor(dataset$holydays_gr_10w)
# 
# 
#   # left_join(gini_country [, c("geo", "values")], by = c("country" = "geo"))  |>
#   # rename(gini_country = values) |>
#   # left_join(migration_country [, c("geo", "values")], by = c("country" = "geo"))  |>
#   # rename(migration_country = values)
# 
# 
# #aggiungi dummies regioni
# dataset$region <- sapply(dataset$nuts_id, assign_region)
# # dataset$region <- factor(dataset$region,
# #                          levels = c("Southern Europe", "Western Europe", "Northern Europe", "Eastern Europe"))
# 
# #solo europa meridionale (da usare sul vettori caratteri non sui fattori)
# for (i in 1:nrow(dataset)) {
#   if(dataset$region[i] != "Southern Europe"){
#     dataset$region[i] <- 0
#   } else {
#     dataset$region[i] <- 1
#   }
# }
# 
# dataset$region <- as.factor(dataset$region)
# 
# saveRDS(dataset, file = "dataset.rds")
# saveRDS(NUTS2, file = "NUTS2.rds")

dataset <- readRDS("data/dataset.rds")
NUTS2 <- readRDS("data/NUTS2.rds")


# escluse
excluded <- filter(dataset, if_any(everything(), is.na))  |> 
  left_join(NUTS2 [, c("geo", "CNTR_CODE")], by = c("nuts_id" = "geo"))  |>
  mutate(Region = nuts_name, Country = CNTR_CODE) |> 
  dplyr::select(Region, Country) 


# Elimina le righe con almeno una cella vuota (NA)
dataset <- dataset  |>
  filter(if_all(everything(), ~ !is.na(.)))

