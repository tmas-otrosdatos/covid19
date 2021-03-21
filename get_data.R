## 

library(sf)
library(ggplot2)
library(tidyverse)
library(tmap)

rm(list=ls())

### Descarga BD de resultados de IHME

# archivos temporales
temp <- tempfile()
temp2 <- tempfile()
# descarga el archivo .shp y se guarda en 'temp' 
download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip",temp)
#descomprimir y guardar en 'temp2'
unzip(zipfile = temp, exdir = temp2)
# encontrar ruta del shapefile (.shp) en temp2
# el $ al final de ".shp$" asegura de no encontrar archivos como: .shp.xml 
ihme <- list.files(temp2, full.names=TRUE)
ihme <- read_csv(file.path(ihme, "Summary_stats_all_locs.csv"))

### Descarga base de datos de SINAVE

url <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
temp <- tempfile()
temp2 <- tempfile()
# descarga el archivo .shp y se guarda en 'temp' 
download.file(url, temp)
#descomprimir y guardar en 'temp2'
unzip(zipfile = temp, exdir = temp2)
# encontrar ruta del shapefile (.shp) en temp2
# el $ al final de ".shp$" asegura de no encontrar archivos como: .shp.xml 
sinave <- read_csv(file.path(temp2, "210319COVID19MEXICO.csv"))

cols <- c("ORIGEN", "SECTOR", "ENTIDAD_UM", "SEXO", "ENTIDAD_NAC", "ENTIDAD_RES", 
          "MUNICIPIO_RES", "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "NACIONALIDAD", 
          "EMBARAZO", "HABLA_LENGUA_INDIG", "INDIGENA", "DIABETES", "EPOC", "ASMA", 
          "INMUSUPR", "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD",
          "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "TOMA_MUESTRA_LAB", "RESULTADO_LAB",
          "TOMA_MUESTRA_ANTIGENO", "RESULTADO_ANTIGENO", "CLASIFICACION_FINAL", "MIGRANTE",
          "PAIS_NACIONALIDAD", "PAIS_ORIGEN", "UCI")

sinave[cols] <- lapply(sinave[cols], factor)

sapply(sinave, class)

rm(cols, ihme, temp, temp2, url)

sinave %>%
  filter(CLASIFICACION_FINAL == 7) %>%
  group_by(FECHA_INGRESO) %>%
  summarize(positivos = n()) %>%
  ggplot() +
  geom_line(aes(x = FECHA_INGRESO, y = positivos))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")
  
library(zoo)


casos_mex <- sinave %>%
  filter(CLASIFICACION_FINAL == 7) %>%
  group_by(FECHA_INGRESO) %>%
  summarize(positivos = n()) %>%
  mutate(positivos_ma01 = rollmean(positivos, k = 2, fill = NA, align = "right"),
         positivos_ma02 = rollmean(positivos, k = 7, fill = NA, align = "right"),
         positivos_ma03 = rollmean(positivos, k = 14, fill = NA, align = "right"))

casos_mex %>%
  gather(metric, value, positivos_ma02:positivos_ma03) %>%
  ggplot(aes(FECHA_INGRESO, value, color = metric)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")


casos_mex %>%
  ggplot(aes(FECHA_INGRESO, positivos_ma03)) +
  geom_line()
