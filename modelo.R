## 

library(sf)
library(ggplot2)
library(tidyverse)
library(tmap)
library(reshape2)
library(plotly)
library(dygraphs)
library(xts)

rm(list=ls())

load(file = paste0("data/", fecha, "sinave.Rda"))

sinave <- sinave %>% 
  mutate(GRUPO_EDAD = case_when(EDAD<=10 ~ '1-10',
                                EDAD>10 & EDAD<=18 ~ '11-18',
                                EDAD>18 & EDAD<=30 ~ '19-30',
                                EDAD>30 & EDAD<=40 ~ '30-40',
                                EDAD>40 & EDAD<=50 ~ '40-50',
                                EDAD>50 & EDAD<=60 ~ '50-60',
                                EDAD>60 ~ 'Mayor de 60')) %>%
  mutate(FALLECIDO = ifelse(is.na(FECHA_DEF), "Recuperado", "Fallecido"))

sinave$CLASIFICACION_FINAL <- forcats::fct_collapse(sinave$CLASIFICACION_FINAL, 
                                                        Confirmado = c("1", "2", "3"), Inválido = c("4", "5"),
                                                        Sospechoso = c("6"), Negativo = c("7"))

sinave20 <- sinave %>%
  filter(FECHA_INGRESO < "2021-01-01")

sinave_model <- sinave20 %>%
  select(ENTIDAD_RES,
         MUNICIPIO_RES,
         NEUMONIA,
         EDAD,
         EMBARAZO,
         INDIGENA,
         DIABETES,
         EPOC,
         ASMA,
         INMUSUPR,
         HIPERTENSION,
         OTRA_COM,
         CARDIOVASCULAR,
         OBESIDAD,
         RENAL_CRONICA,
         TABAQUISMO,
         CLASIFICACION_FINAL,
         GRUPO_EDAD)

sinave_model <- sinave_model %>%
  filter(CLASIFICACION_FINAL == "Confirmado")

sinave_model %>% group_by(GRUPO_EDAD) %>%
  summarize(n())

write_csv(sinave_model, "data/sinave_model.csv")
