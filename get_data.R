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
start <- Sys.time()
### Descarga base de datos de SINAVE

url <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
temp <- tempfile()                      # crea un archivo temporal para el zip
temp2 <- tempfile()                     # crea un archivo temporal para el unzip
download.file(url, temp)                # descarga archivo
unzip(zipfile = temp, exdir = temp2)    # descomprime archivo y guarda en temp2

# Guarda la fecha de ayer en la variable "fecha"
fecha <- paste0(format(Sys.Date(), "%y"), format(Sys.Date(), "%m"), format(Sys.Date()-1, "%d"))
fecha_print <- format(Sys.Date()-1, "%d-%m-%Y")

# Lee el archivo compuesto por la fecha y el identificador general
sinave <- read_csv(file.path(temp2, paste0(fecha, "COVID19MEXICO.csv")))

# Vector de columnas que se cambian a factor
cols <- c("ORIGEN", "SECTOR", "ENTIDAD_UM", "SEXO", "ENTIDAD_NAC", "ENTIDAD_RES", 
          "MUNICIPIO_RES", "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "NACIONALIDAD", 
          "EMBARAZO", "HABLA_LENGUA_INDIG", "INDIGENA", "DIABETES", "EPOC", "ASMA", 
          "INMUSUPR", "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD",
          "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "TOMA_MUESTRA_LAB", "RESULTADO_LAB",
          "TOMA_MUESTRA_ANTIGENO", "RESULTADO_ANTIGENO", "CLASIFICACION_FINAL", "MIGRANTE",
          "PAIS_NACIONALIDAD", "PAIS_ORIGEN", "UCI")

# Aplica la función factor() al vector de columnas
sinave[cols] <- lapply(sinave[cols], factor)

rm(cols, temp, temp2, url)    # Limpia las variables que no se utilizan

save(sinave, file = paste0("data/", fecha, "sinave.Rda"))

sinave <- sinave %>% 
  mutate(GRUPO_EDAD = case_when(EDAD<=10 ~ '1-10',
                                EDAD>10 & EDAD<=18 ~ '11-18',
                                EDAD>18 & EDAD<=30 ~ '19-30',
                                EDAD>30 & EDAD<=40 ~ '30-40',
                                EDAD>40 & EDAD<=50 ~ '40-50',
                                EDAD>50 & EDAD<=60 ~ '50-60',
                                EDAD>60 ~ 'Mayor de 60'))

### Todos
sinave_all <- sinave %>%
  mutate(FALLECIDO = ifelse(is.na(FECHA_DEF), "Recuperado", "Fallecido")) %>%
  group_by(FECHA_INGRESO, FECHA_DEF, SECTOR, ENTIDAD_UM, TIPO_PACIENTE, INTUBADO, EDAD, 
           CLASIFICACION_FINAL, FALLECIDO) %>%
  summarise(total_casos = n())

sinave_all$CLASIFICACION_FINAL <- forcats::fct_collapse(sinave_all$CLASIFICACION_FINAL, 
                                                       Confirmado = c("1", "2", "3"), Inválido = c("4", "5"),
                                                       Sospechoso = c("6"), Negativo = c("7"))
#labs <- c(paste(seq(0, 55, by = 10), seq(0 + 10 - 1, 60 - 1, by = 10),
#                sep = "-"), paste(60, "+", sep = ""))

#labs

#sinave_all$GRUPO_EDAD <- cut(sinave_all$EDAD, breaks = c(seq(0, 60, by = 10), Inf), labels = labs, right = FALSE)

sinave_all$SECTOR <- forcats::fct_collapse(sinave_all$SECTOR,
                                           "Cruz Roja" = c("1"), "DIF" = c("2"), "Estatal" = c("3"),
                                           "IMSS" = c("4", "5"), "ISSSTE" = c("6"), "Municipal" = c("7"),
                                           "PEMEX" = c("8"), "Privada" = c("9"), "Militar" = c("10", "11"),
                                           "SSA" = c("12"), "Universitario" = c("13"), "No especificado" = c("14", "99"))

sinave_all$INTUBADO <- forcats::fct_collapse(sinave_all$INTUBADO,
                                             "Intubado" = "1", "No intubado" = "2", "No aplica" = "97", "Se ingora" = "98",
                                             "No especificado" = "99")

sinave_all$TIPO_PACIENTE <- forcats::fct_collapse(sinave_all$TIPO_PACIENTE,
                                                  "Ambulatorio" = "1", "Hospitalizado" = "2", "No especificado" = "99")

sinave_all$ENTIDAD_UM <- forcats::fct_collapse(sinave_all$ENTIDAD_UM,
                                                  "Aguascalientes" = "01", "Baja California" = "02", "Baja California Sur" = "03",
                                               "Campeche" = "04", "Coahuila" = "05", "Colima" = "06", "Chiapas" = "07", "Chihuahua" = "08", "CDMX" = "09", "Durango" = "10", "Guanajuato" = "11", "Guerrero" = "12", "Hidalgo" = "13", "Jalisco" = "14", "México" = "15", "Michoacán" = "16", "Morelos" = "17", "Nayarit" = "18", "Nuevo León" = "19", "Oaxaca" = "20", "Puebla" = "21", "Querétaro" = "22", "Quintana Roo" = "23", "San Luis Potosí" = "24", "Sinaloa" = "25", "Sonora" = "26", "Tabasco" = "27", "Tamaulipas" = "28", "Tlaxcala" = "29", "Veracruz" = "30", "Yucatán" = "31", "Zacatecas" = "32")

sinave_csv <- sinave_all %>%
  group_by(FECHA_INGRESO, FECHA_DEF, SECTOR, ENTIDAD_UM, TIPO_PACIENTE, INTUBADO, GRUPO_EDAD, CLASIFICACION_FINAL,
           FALLECIDO) %>%
  summarise(total = sum(total_casos))

write_csv(sinave_csv, "data/sinave_agegroup.csv")

write_csv(sinave_all, "data/sinave_full.csv")

sinave_60 <- sinave_csv %>%
  filter(GRUPO_EDAD == "60+")

write_csv(sinave_60, "data/sinave_60.csv")

end <- Sys.time()

time1 <- end - start

#sinave_age_entidad <- sinave_csv %>%
#  filter(CLASIFICACION_FINAL == "confirmado") %>%
#  group_by(FECHA_INGRESO, FALLECIDO, GRUPO_EDAD) %>%
#  summarize(positivos = n()) %>%
#  mutate(positivos_ma01 = rollmean(positivos, k = 2, fill = NA, align = "right"),
#         positivos_ma02 = rollmean(positivos, k = 7, fill = NA, align = "right"),
#         positivos_ma03 = rollmean(positivos, k = 14, fill = NA, align = "right"))


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("figs/Original on Transparent.png")
t <- grid::roundrectGrob()

# 1. Total de casos por resultado de prueba
sinave_csv %>%
  group_by(SECTOR) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

sinave_class <- sinave_csv %>%
  group_by(CLASIFICACION_FINAL) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

png(file="figs/post1/01pruebas.png",
    width=870, height=550)

sinave_class %>%
  ggplot(aes(x = reorder(CLASIFICACION_FINAL, total), y = total, 
             label = format(total, big.mark = ",", scientific = FALSE), 
             color = CLASIFICACION_FINAL, 
             fill = CLASIFICACION_FINAL)) +
  geom_col() +
  geom_text(nudge_y = 100000, size = 5, color = "black", fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total de casos por resultado de prueba", x = "Resultado de prueba", y = "Total de casos", 
       caption = paste0("Con información de SINAVE al ", fecha_print),
       subtitle = "COVID-19 en México") +
  theme_void() +
  scale_fill_brewer(palette = "PuBuGn", direction = -1) +
  scale_color_brewer(palette = "PuBuGn", direction = -1) +
  theme(legend.position = "none", plot.title = element_text(size=20),
        axis.text.x = element_text(size = 12),
        text = element_text(size=16),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.margin = unit(c(1, 1, 3, 1), "lines"))+
  annotate("text", x = "Inválido", y = 2000000, 
           label = paste0(round(sinave_class[sinave_class$CLASIFICACION_FINAL == "Confirmado", "pct"]*100,0), 
                          "% del total de casos registrados\n", "ha sido confirmado como positivo\n", 
                          "a COVID-19"), 
           alpha = .8, size = 6, color = 'black', hjust = 0)+
  annotation_custom(l, xmin = 0.5, xmax = 1.5, ymin = -1000000, ymax = -500000) +
  coord_cartesian(clip = "off")

dev.off()

# 2. Casos positivos por grupo de edad
sinave_edad_fallecidos <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  group_by(GRUPO_EDAD, FALLECIDO) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

sinave_edad_fallecidos_2 <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  group_by(FALLECIDO, GRUPO_EDAD) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

sinave_resumen_edad <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  group_by(GRUPO_EDAD) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

contagiados_vulnerables <- sinave_resumen_edad %>%
  filter(GRUPO_EDAD == "60+" | GRUPO_EDAD == "50-59") %>%
  summarise(sum(pct))

no_vulnerables <- round((1 - contagiados_vulnerables)*100,0)

png(file="figs/post1/02resultado_edad.png",
    width=1000, height=550)

sinave_edad_fallecidos %>%
  ggplot(aes(x = GRUPO_EDAD, y = total, fill = FALLECIDO, color = FALLECIDO,
             label = format(total, big.mark = ",", scientific = FALSE))) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(0.9), size = 4, vjust = -0.8, hjust = 0.5, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Resultado de casos positivos por grupo de edad", x = "Grupo de edad", y = "Total de casos", 
       fill = "Resultado", caption = paste0("Con información de SINAVE al ", fecha_print),
       subtitle = "COVID-19 en México")+
  guides(color = FALSE) +
  theme_classic() +
  scale_fill_brewer(palette = "PuBuGn", direction = -1) +
  scale_color_brewer(palette = "PuBuGn", direction = -1) +
  theme(plot.title = element_text(size=20),
        axis.text.x = element_text(size = 12),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.margin = unit(c(1, 1, 3, 1), "lines"))+
  annotate("text", x = "0-9", y = 450000, 
           label = paste0(round(sinave_edad_fallecidos[sinave_edad_fallecidos$GRUPO_EDAD == "60+" & 
                                                         sinave_edad_fallecidos$FALLECIDO == "Fallecido", "pct"]*100,0), 
                          "% de los adultos\n", "mayores falleció"), 
           size = 5, color = 'black', hjust = 0)+
  annotate("text", x = "0-9", y = 350000, 
           label = paste0(round(sinave_edad_fallecidos_2[sinave_edad_fallecidos_2$GRUPO_EDAD == "60+" & 
                                                           sinave_edad_fallecidos_2$FALLECIDO == "Fallecido", "pct"]*100,0), 
                          "% de las defunciones\n", "son de adultos mayores"), 
           size = 5, color = 'black', hjust = 0)+
  annotate("text", x = "0-9", y = 250000, 
           label = paste0(no_vulnerables, 
                          "% de los contagios\n", "son en menores de 49 años"), 
           size = 5, color = 'black', hjust = 0)+
  annotation_custom(l, xmin = 0.5, xmax = 1.5, ymin = -100000, ymax = -60000) +
  coord_cartesian(clip = "off")

dev.off()



# 3.1 Total de casos por sector

sinave_resumen_edad_class <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado" | CLASIFICACION_FINAL == "Negativo") %>%
  group_by(GRUPO_EDAD, CLASIFICACION_FINAL) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))


png(file="figs/post1/03casos_edad.png",
    width=870, height=550)

sinave_resumen_edad_class %>%
  ggplot(aes(x = GRUPO_EDAD, 
             y = total, 
             fill = CLASIFICACION_FINAL)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total de casos por grupo de edad", x = "Grupo de edad", y = "Total de casos", fill = "Resultado de prueba", 
       caption = paste0("Con información de SINAVE al ", fecha_print),
       subtitle = "COVID-19 en México") +
  guides(color = FALSE) +
  theme_classic() +
  scale_fill_brewer(palette = "PuBuGn") +
  scale_color_brewer(palette = "PuBuGn") +
  theme(plot.title = element_text(size=20),
        axis.text.x = element_text(size = 12),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.margin = unit(c(1, 1, 3, 1), "lines")) +
  annotation_custom(l, xmin = 0.5, xmax = 1.5, ymin = -300000, ymax = -200000) +
  coord_cartesian(clip = "off")

dev.off()

# 4.1 Tipo de paciente por sector

#sinave_resumen_pac_sector <- sinave_csv %>%
#  filter(CLASIFICACION_FINAL == "confirmado") %>%
#  group_by(SECTOR, TIPO_PACIENTE) %>%
#  summarise(total = sum(total)) %>%
#  mutate(pct = prop.table(total))
#
#sinave_resumen_pac_sector %>%
#  group_by(TIPO_PACIENTE) %>%
#  summarise(promedio = mean(pct))
#
#png(file="figs/post1/04paciente_sector.png",
#    width=870, height=550)
#sinave_resumen_pac_sector %>%
#  ggplot() +
#  geom_col(aes(x = reorder(SECTOR, total), y = total, fill = TIPO_PACIENTE)) +
#  coord_flip() +
#  scale_y_continuous(labels = scales::comma) +
#  labs(title = "Total de casos por tipo de paciente en sectores de salud", x = "Sector", y = "Total de casos", 
#       fill = "Tipo de paciente") +
#  theme_minimal()
#dev.off()

# 4.1.2 Tipo de paciente por sector

sinave_resumen_pac_sector <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  group_by(TIPO_PACIENTE, SECTOR) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

sinave_resumen_sector_pac <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  group_by(SECTOR, TIPO_PACIENTE) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

sinave_resumen_pac_sector %>%
  group_by(TIPO_PACIENTE) %>%
  summarise(promedio = mean(pct))

png(file="figs/post1/04paciente_sector_2.png",
    width=870, height=550)

sinave_resumen_sector_pac %>%
  filter(SECTOR != "No especificado" & SECTOR != "Cruz Roja" & SECTOR != "DIF") %>%
  ggplot() +
  geom_col(aes(x = reorder(SECTOR, -total), y = total, fill = TIPO_PACIENTE)) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total de casos por tipo de paciente en sectores de salud", x = "Sector", y = "Total de casos", 
       fill = "Tipo de paciente", caption = paste0("Con información de SINAVE al ", fecha_print),
       subtitle = "COVID-19 en México") +
  theme_classic() +
  scale_fill_brewer(palette = "PuBuGn") +
  scale_color_brewer(palette = "PuBuGn") +
  theme(plot.title = element_text(size=20),
        axis.text.x = element_text(size = 12),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.margin = unit(c(1, 1, 3, 1), "lines")) +
  annotate("text", x = "ISSSTE", y = 450000, 
           label = paste0("El IMSS tiene el ", 
                          round(sinave_resumen_pac_sector[sinave_resumen_pac_sector$SECTOR == "IMSS" & 
                                                            sinave_resumen_pac_sector$TIPO_PACIENTE == "Hospitalizado", "pct"]*100,0), 
                          "% de total\n", "de pacientes hospitalizados"), 
           size = 5, color = 'black', hjust = 0) +
  annotation_custom(l, xmin = 0.5, xmax = 1.5, ymin = -250000, ymax = -150000) +
  coord_cartesian(clip = "off")

dev.off()

# 4.2 Tipo de paciente por grupo de edad en sectores principales

#sinave_resumen_tipo_edad <- sinave_csv %>%
#  filter(CLASIFICACION_FINAL == "confirmado") %>%
#  filter(SECTOR == "IMSS" | SECTOR == "Militar" | SECTOR == "Privada"
#         | SECTOR == "SSA"| SECTOR == "ISSSTE") %>%
#  group_by(SECTOR, GRUPO_EDAD, TIPO_PACIENTE) %>%
#  summarise(total = sum(total)) %>%
#  mutate(pct = prop.table(total))
#
#png(file="figs/post1/04casos_sector.png",
#    width=1500, height=550)
#sinave_resumen_tipo_edad %>%
#  ggplot(aes(x = GRUPO_EDAD, 
#             y = total, 
#             fill = TIPO_PACIENTE, 
#             label = paste0(round(pct*100,1),'%'))) +
#  geom_col() +
#  geom_text(data = subset(sinave_resumen_tipo_edad, GRUPO_EDAD != "0-9" & GRUPO_EDAD != "10-19"), 
#            size = 3, 
#            colour = "white", 
#            position=position_stack(vjust=0.9), fontface = "bold") +
#  facet_wrap(.~SECTOR, scales = "free", ncol = 5) +
#  scale_y_continuous(labels = scales::comma) +
#  theme_minimal() +
#  labs(title = "Total de casos por tipo de paciente y grupo de edad en sectores de salud", 
#       x = "Grupo de edad", y = "Total de casos",
#       fill = "Tipo de paciente", caption = paste0("Con información de SINAVE al ", fecha_print)) +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#dev.off()

# 4.2.2 Tipo de paciente por grupo de edad en sectores principales

sinave_resumen_tipo_edad <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  filter(SECTOR == "IMSS" | SECTOR == "Privada"
         | SECTOR == "SSA") %>%
  group_by(SECTOR, GRUPO_EDAD, TIPO_PACIENTE) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

png(file="figs/post1/04casos_sector_2.png",
    width=1000, height=550)

sinave_resumen_tipo_edad %>%
  ggplot(aes(x = GRUPO_EDAD, 
             y = total, 
             fill = TIPO_PACIENTE, 
             label = paste0(round(pct*100,0),'%'))) +
  geom_col() +
  geom_text(data = subset(sinave_resumen_tipo_edad, total > 15000), 
            size = 3, 
            colour = "black", 
            position=position_stack(vjust=0.9), fontface = "bold") +
  facet_wrap(.~SECTOR, ncol = 3) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Total de casos por tipo de paciente y grupo de edad en sectores de salud", 
       x = "Grupo de edad", y = "Total de casos",
       fill = "Tipo de paciente", caption = paste0("Con información de SINAVE al ", fecha_print),
       subtitle = "COVID-19 en México") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "PuBuGn") +
  scale_color_brewer(palette = "PuBuGn") +
  theme(plot.title = element_text(size=20),
        axis.text.x = element_text(size = 12),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.margin = unit(c(1, 1, 3, 1), "lines")) 
#+
#  annotation_custom(l, xmin = 0.5, xmax = 1.5, ymin = -100000, ymax = -50000) +
#  coord_cartesian(clip = "off")

dev.off()

# 4.3 Resultado de casos por grupo de edad y sector
#
#sinave_resumen_fallecido <- sinave_csv %>%
#  filter(CLASIFICACION_FINAL == "confirmado") %>%
#  filter(SECTOR == "IMSS" | SECTOR == "Militar" | SECTOR == "Privada"
#         | SECTOR == "SSA"| SECTOR == "ISSSTE") %>%
#  group_by(SECTOR, GRUPO_EDAD, FALLECIDO) %>%
#  summarise(total = sum(total)) %>%
#  mutate(pct = prop.table(total))
#
#png(file="figs/post1/04edad_sector.png",
#    width=1500, height=550)
#sinave_resumen_fallecido %>%
#  ggplot(aes(x = GRUPO_EDAD, 
#             y = total, 
#             fill = FALLECIDO,
#             label = paste0(round(pct*100,1),'%'))) +
#  geom_col() +
#  geom_text(data = subset(sinave_resumen_fallecido, GRUPO_EDAD != "0-9" & GRUPO_EDAD != "10-19"), 
#            size = 3, 
#            colour = "white", 
#            position=position_stack(vjust=0.9), fontface = "bold") +
#  facet_wrap(.~SECTOR, scales = "free", ncol = 5) +
#  scale_y_continuous(labels = scales::comma) +
#  theme_minimal() +
#  labs(title = "Resultado de casos positivos por grupo de edad y sector", x = "Grupo de edad", y = "Total de casos",
#       fill = "Resultado", caption = paste0("Con información de SINAVE al ", fecha_print)) +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#dev.off()

# 4.3.1 Resultado de casos por grupo de edad y sector

sinave_resumen_fallecido <- sinave_csv %>%
  filter(CLASIFICACION_FINAL == "Confirmado") %>%
  filter(SECTOR == "IMSS" | SECTOR == "Privada"
         | SECTOR == "SSA") %>%
  group_by(SECTOR, GRUPO_EDAD, FALLECIDO) %>%
  summarise(total = sum(total)) %>%
  mutate(pct = prop.table(total))

png(file="figs/post1/04edad_sector_2.png",
    width=1000, height=550)

sinave_resumen_fallecido %>%
  ggplot(aes(x = GRUPO_EDAD, 
             y = total, 
             fill = FALLECIDO,
             label = paste0(round(pct*100,0),'%'))) +
  geom_col() +
  geom_text(data = subset(sinave_resumen_fallecido, total > 15000), 
            size = 3, 
            colour = "black", 
            position=position_stack(vjust=0.9), fontface = "bold") +
  facet_wrap(.~SECTOR, ncol = 3) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Resultado de casos positivos por grupo de edad y sector", x = "Grupo de edad", y = "Total de casos",
       fill = "Resultado", caption = paste0("Con información de SINAVE al ", fecha_print),
       subtitle = "COVID-19 en México") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "PuBuGn", direction = -1) +
  scale_color_brewer(palette = "PuBuGn") +
  theme(plot.title = element_text(size=20),
        axis.text.x = element_text(size = 12),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        plot.margin = unit(c(1, 1, 3, 1), "lines")) 

dev.off()


end2 <- Sys.time()


end-start
end2-start





# Casos positivos por grupo de edad y tipo de paciente en sectores principales
sinave_csv %>%
  filter(CLASIFICACION_FINAL == "confirmado") %>%
  filter(SECTOR == "IMSS" | SECTOR == "Militar" | SECTOR == "Privada"
         | SECTOR == "SSA"| SECTOR == "ISSSTE") %>%
  group_by(GRUPO_EDAD, FALLECIDO, TIPO_PACIENTE, SECTOR) %>%
  summarise(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = GRUPO_EDAD, y = total, fill = FALLECIDO)) +
  facet_grid(TIPO_PACIENTE~SECTOR) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(title = "Resultado de casos positivos por grupo de edad y tipo de paciente en sectores principales", x = "Grupo de edad", y = "Total de casos",
       fill = "Resultado")

# Casos positivos por grupo de edad e intubados en sectores principales
sinave_csv %>%
  filter(CLASIFICACION_FINAL == "confirmado") %>%
  filter(SECTOR == "IMSS" | SECTOR == "Militar" | SECTOR == "Privada"
         | SECTOR == "SSA"| SECTOR == "ISSSTE") %>%
  filter(TIPO_PACIENTE == "Hospitalizado") %>%
  group_by(GRUPO_EDAD, FALLECIDO, INTUBADO, SECTOR) %>%
  summarise(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = GRUPO_EDAD, y = total, fill = FALLECIDO)) +
  facet_grid(INTUBADO~SECTOR) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(title = "Resultado de casos positivos por grupo de edad intubados en sectores principales", x = "Grupo de edad", y = "Total de casos",
       fill = "Resultado")

# 
sinave_csv %>%
  filter(CLASIFICACION_FINAL == "confirmado") %>%
  group_by(GRUPO_EDAD, FALLECIDO, ENTIDAD_UM) %>%
  summarise(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = GRUPO_EDAD, y = total, fill = FALLECIDO)) +
  facet_wrap(.~ENTIDAD_UM) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(title = "Resultado de casos positivos por grupo de edad y estado", x = "Grupo de edad", y = "Total de casos",
       fill = "Resultado")



sinave_csv %>%
  filter(CLASIFICACION_FINAL == "confirmado") %>%
  filter(SECTOR == "IMSS" | SECTOR == "Militar" | SECTOR == "Privada"
         | SECTOR == "SSA") %>%
  group_by(GRUPO_EDAD, FALLECIDO, TIPO_PACIENTE, SECTOR) %>%
  summarise(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = GRUPO_EDAD, y = total, fill = FALLECIDO)) +
  facet_grid(TIPO_PACIENTE~SECTOR) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

sinave_csv %>%
  filter(CLASIFICACION_FINAL != "invalido") %>%
  group_by(GRUPO_EDAD, SECTOR, CLASIFICACION_FINAL, TIPO_PACIENTE) %>%
  summarise(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = GRUPO_EDAD, y = total, fill = TIPO_PACIENTE), position = "stack") +
  facet_wrap(.~CLASIFICACION_FINAL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
  
















#sinave_age_entidad <- sinave_csv %>%
#  dplyr::arrange(desc(GRUPO_EDAD)) %>%
#  dplyr::group_by(FECHA_INGRESO, GRUPO_EDAD, ENTIDAD_UM) %>%
#  dplyr::mutate(total_7 = zoo::rollmean(total, k = 7, fill = NA),
#                total_15 = zoo::rollmean(total, k = 15, fill = NA)) %>%
#  select(FECHA_INGRESO, GRUPO_EDAD, ENTIDAD_UM, total_7, total_15)
#
#
#sinave_age_sector_fallecidos <- sinave_csv %>%
#  dplyr::arrange(desc(GRUPO_EDAD)) %>%
#  dplyr::group_by(FECHA_INGRESO, GRUPO_EDAD, SECTOR, FALLECIDO) %>%
#  dplyr::mutate(total_7 = zoo::rollmean(total, k = 7, fill = NA),
#                total_15 = zoo::rollmean(total, k = 15, fill = NA)) %>%
#  select(FECHA_INGRESO, GRUPO_EDAD, SECTOR, FALLECIDO, total)
#
#sinave_age_sector_fallecidos <- sinave_csv %>%
#  dplyr::arrange(desc(GRUPO_EDAD)) %>%
#  dplyr::group_by(FECHA_INGRESO, GRUPO_EDAD, SECTOR, FALLECIDO) %>%
#  select(FECHA_INGRESO, GRUPO_EDAD, SECTOR, FALLECIDO, total)
#
#sinave_age_sector_fallecidos %>%
#  ggplot() +
#  geom_line(aes(x = FECHA_INGRESO, y = total_7, col = FALLECIDO)) +
#  facet_grid(GRUPO_EDAD~SECTOR)
#
#
#
#test <- sinave_ma %>%
#  filter(CLASIFICACION_FINAL == "confirmado" | SECTOR == "IMSS") %>%
#  group_by(FECHA_INGRESO, GRUPO_EDAD, INTUBADO, ENTIDAD_UM, TIPO_PACIENTE, FALLECIDO, SECTOR) %>%
#  summarise(ma_15 = sum(total_15))
#
#sinave_ma %>%
#  filter(CLASIFICACION_FINAL == "confirmado" | SECTOR == "IMSS") %>%
#  group_by(FECHA_INGRESO, GRUPO_EDAD, INTUBADO, ENTIDAD_UM, TIPO_PACIENTE, FALLECIDO, SECTOR) %>%
#  summarise(ma_15 = sum(total_15)) %>%
#  ggplot() +
#  geom_line(aes(x = FECHA_INGRESO, y = ma_15, col = GRUPO_EDAD)) +
#  facet_grid(TIPO_PACIENTE~FALLECIDO)
#
#test %>%
#  filter(GRUPO_EDAD == "60+") %>%
#  group_by(FECHA_INGRESO, GRUPO_EDAD, INTUBADO, ENTIDAD_UM, TIPO_PACIENTE, FALLECIDO, SECTOR) %>%
#  ggplot() +
#  geom_line(aes(x = FECHA_INGRESO, y = ma_15, col = SECTOR)) +
#  facet_grid(TIPO_PACIENTE~FALLECIDO)
#
#sinave_ma %>%
#  filter(GRUPO_EDAD =="60+")
#
#
#
#
#
#
#
#
#sinave_all %>% group_by(CLASIFICACION_FINAL, FALLECIDO) %>%
#  summarise(total = sum(total_casos))
#
#sector_all <- sinave_all %>% group_by(FALLECIDO, SECTOR) %>%
#  summarise(total = sum(total_casos))
#
#sector_all %>% ggplot() +
#  geom_col(aes(x = SECTOR, y = total, fill = FALLECIDO), position = "fill")



#letalidad <- sinave_all %>%
#  group_by(SECTOR, FALLECIDO) %>%
#  summarise(letalidad = sum(total_casos))
#
#
#### +60
#sinave_60 <- sinave %>%
#  filter(EDAD > 60) %>%
#  mutate(FALLECIDO = ifelse(is.na(FECHA_DEF), "recuperado", "fallecido")) %>%
#  group_by(FECHA_INGRESO, FECHA_DEF, SECTOR, ENTIDAD_UM, TIPO_PACIENTE, CLASIFICACION_FINAL, FALLECIDO) %>%
#  summarise(total_casos = n())
#
#sinave_60$CLASIFICACION_FINAL <- forcats::fct_collapse(sinave_60$CLASIFICACION_FINAL, 
#                                                       confirmado = c("1", "2", "3"), invalido = c("4", "5"),
#                                                       sospechoso = c("6"), negativo = c("7"))
#
#sinave_60$SECTOR <- forcats::fct_collapse(sinave_60$SECTOR,
#                                          "Cruz Roja" = c("1"), "DIF" = c("2"), "Estatal" = c("3"),
#                                          "IMSS" = c("4", "5"), "ISSSTE" = c("6"), "Municipal" = c("7"),
#                                          "PEMEX" = c("8"), "Privada" = c("9"), "Militar" = c("10", "11"),
#                                          "SSA" = c("12"), "Universitario" = c("13"), "No especificado" = c("14", "99"))
#
#sinave_60 %>% group_by(CLASIFICACION_FINAL, FALLECIDO) %>%
#  summarise(total = sum(total_casos))
#
#sector <- sinave_60 %>% group_by(FALLECIDO, SECTOR) %>%
#  summarise(total = sum(total_casos))
#
#sector %>% ggplot() +
#  geom_col(aes(x = SECTOR, y = total, fill = FALLECIDO), position = "stack")
#
#letalidad_60 <- sinave_60 %>%
#  group_by(SECTOR, FALLECIDO) %>%
#  summarise(letalidad = sum(total_casos))

### Positividad
pruebas <- sinave %>%
  filter(TOMA_MUESTRA_LAB == 1 | TOMA_MUESTRA_ANTIGENO == 1) %>%
  group_by(FECHA_INGRESO) %>%
  summarize(pruebas_tot = n())

pruebas_pos <- sinave %>%
  filter(RESULTADO_LAB == 1 | RESULTADO_ANTIGENO == 1) %>%
  group_by(FECHA_INGRESO) %>%
  summarize(pruebas_pos = n())

positividad <- inner_join(pruebas, pruebas_pos, by = "FECHA_INGRESO")

positividad <- positividad %>%
  mutate(porcentaje = pruebas_pos/pruebas_tot)

pos_long <- positividad %>%
  gather(key = "FECHA_INGRESO", value = valor, pruebas_tot:porcentaje)

positividad %>% 
  ggplot() +
  geom_line(aes(x = FECHA_INGRESO, y = rollmean(pruebas_tot, k = 14, fill = NA, align = "right")))+
  geom_line(aes(x = FECHA_INGRESO, y = rollmean(pruebas_pos, k = 14, fill = NA, align = "right")))

# Genera media móvil 
casos_mex <- sinave %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  group_by(FECHA_INGRESO) %>%
  summarize(positivos = n()) %>%
  mutate(positivos_ma01 = rollmean(positivos, k = 2, fill = NA, align = "right"),
         positivos_ma02 = rollmean(positivos, k = 7, fill = NA, align = "right"),
         positivos_ma03 = rollmean(positivos, k = 14, fill = NA, align = "right"))

sospechosos <- sinave %>%
  filter(CLASIFICACION_FINAL == 6, CLASIFICACION_FINAL == 4, CLASIFICACION_FINAL == 5) %>%
  summarize(sospechosos = n()) *	0.13705003

estimados <- casos_mex %>%
  summarize(total = sum(positivos))

sospechosos + estimados

defunciones <- sinave %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  filter(!is.na(FECHA_DEF))

write_csv(defunciones, paste0(fecha, "defunciones.csv"))


def_long <- melt(defunciones, id=c("FECHA_ACTUALIZACION","ID_REGISTRO", 
                                   "FECHA_INGRESO", "FECHA_SINTOMAS", "FECHA_DEF"))

def_long %>% 
  filter(variable == "EDAD")

defunciones %>% ggplot() +
  geom_density(aes(x = EDAD, color = SEXO, fill = INTUBADO), alpha = 0.3) +
  theme_minimal()


defunciones %>% ggplot() +
  stat_bin(aes(x = EDAD, fill = TIPO_PACIENTE), position = "stack") +
  theme_minimal()

def_long %>% 
  filter(variable == "EDAD" | variable == "SEXO") %>%
  summarize() %>%
  group_by(as.factor(value)) %>%
  ggplot() +
  geom_boxplot(aes(x = variable, y = as.factor(value)))

sinave %>%
  ggplot() +
  geom_boxplot(aes(x = SEXO, y = EDAD, color = TOMA_MUESTRA_LAB))

muertes <- sinave %>%
  filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  filter(!is.na(FECHA_DEF)) %>%
  group_by(FECHA_DEF) %>%
  summarize(muertes = n()) %>%
  mutate(muertes_ma01 = rollmean(muertes, k = 2, fill = NA, align = "right"),
         muertes_ma02 = rollmean(muertes, k = 7, fill = NA, align = "right"),
         muertes_ma03 = rollmean(muertes, k = 14, fill = NA, align = "right"))

muertes %>%
  summarize(total = sum(muertes))

casos_mex %>%
  gather(metric, value, positivos:positivos_ma03) %>%
  ggplot(aes(FECHA_INGRESO, value, color = metric)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

muertes %>%
  gather(metric, value, muertes_ma02:muertes_ma03) %>%
  ggplot(aes(FECHA_DEF, value, color = metric)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

casos_mex %>%
  ggplot(aes(FECHA_INGRESO, positivos_ma03)) +
  geom_line()

# Then you can create the xts necessary to use dygraph
casos_diarios <- xts(x = casos_mex$positivos_ma03, order.by = casos_mex$FECHA_INGRESO)

fallecimientos <- xts(x = muertes$muertes_ma03, order.by = muertes$FECHA_DEF)

# Finally the plot
dygraph(casos_diarios) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

dygraph(fallecimientos) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

letalidad <- inner_join(casos_mex, muertes, by = c("FECHA_INGRESO" = "FECHA_DEF"))

letalidad <- letalidad %>%
  mutate(letalidad = muertes_ma03/positivos_ma03*100)

let <- xts(x = letalidad$letalidad, order.by = letalidad$FECHA_INGRESO)


# Pronóstico sin vacuna

data <- letalidad %>% select(FECHA_INGRESO, positivos_ma03, muertes_ma03)

model_data <- subset(data, FECHA_INGRESO < "2021-04-23")

fit <- lm(muertes_ma03 ~ positivos_ma03, data = model_data)

summary(fit)

new <- data.frame(positivos_ma03 = data$positivos_ma03)

pred.int <- predict(fit, newdata = new, interval = "confidence")

casos_proy <- cbind(letalidad, pred.int)


muertes_obs <- xts(x = casos_proy$muertes_ma03, order.by = casos_proy$FECHA_INGRESO)
muertes_pry <- xts(x = casos_proy$fit, order.by = casos_proy$FECHA_INGRESO)

muertes_comp <- cbind(muertes_obs, muertes_pry)

dygraph(muertes_comp, main = "Muertes observadas vs muertes proyectadas")%>%
  dyAxis("y", label = "Muertes", independentTicks = TRUE)%>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1) %>%
  dyEvent(x = "2021-04-24", label = "Inicio de vacunación 60+", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-05-25", label = "Inicio de vacunación 50 a 59", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-06-22", label = "Inicio de vacunación 40 a 49", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-07-18", label = "Inicio de vacunación 30 a 39", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") 

casos_let <- cbind(casos_diarios, let)

dygraph(casos_let, main = "Letalidad por COVID-19 en México")%>%
  dyAxis("y", label = "Casos diarios", independentTicks = TRUE)%>%
  dyAxis("y2", label = "% de letalidad", independentTicks = TRUE) %>%
  dySeries("casos_diarios", axis=('y')) %>%
  dySeries("letalidad", axis=('y2')) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1) %>%
  dyEvent(x = "2021-04-24", label = "Inicio de vacunación 60+", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-05-25", label = "Inicio de vacunación 50 a 59", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-06-22", label = "Inicio de vacunación 40 a 49", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-07-18", label = "Inicio de vacunación 30 a 39", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") 


casos_muertes <- cbind(casos_diarios, fallecimientos)


dygraph(casos_muertes, main = "Letalidad por COVID-19 en México")%>%
  dyAxis("y", label = "Casos diarios", independentTicks = TRUE)%>%
  dyAxis("y2", label = "Fallecimientos", independentTicks = TRUE) %>%
  dySeries("casos_diarios", axis=('y')) %>%
  dySeries("fallecimientos", axis=('y2')) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1) %>%
  dyEvent(x = "2021-04-24", label = "Inicio de vacunación 60+", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-05-25", label = "Inicio de vacunación 50 a 59", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-06-22", label = "Inicio de vacunación 40 a 49", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") %>%
  dyEvent(x = "2021-07-18", label = "Inicio de vacunación 30 a 39", labelLoc = "bottom",
          color = "black", strokePattern = "dashed") 




dygraph(mov_cases, main = "Movilidad por trabajo y recreación/consumo vs casos Covid-19 confirmados en México")%>%
  dyAxis("y", label = "Casos diarios", valueRange = c(0, 20000.0), independentTicks = TRUE)%>%
  dyAxis("y2", label = "Cambio en movilidad (base 100)", valueRange = c(-60, 50), independentTicks = TRUE) %>%
  dySeries("mov_rec", axis=('y2')) %>%
  dySeries("mov_work", axis=('y2'))

library(COVID19)


gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
x <- covid19(country = "MEX", gmr = gmr)


y_work <- x %>% mutate(workplaces = rollmean(workplaces_percent_change_from_baseline, 
                                   k = 14, fill = NA, align = "right"))

y_work %>% ggplot(aes(x = date, y = workplaces))+
  geom_line()


mov_work <- xts(x = y_work$workplaces, order.by = y_work$date)

mov_cases_work <- cbind(casos_diarios, mov_work)

dygraph(mov_cases_work, main = "Movilidad Laboral vs casos Covid-19 confirmados en México")%>%
  dyAxis("y", label = "Casos diarios", valueRange = c(0, 20000.0), independentTicks = TRUE)%>%
  dyAxis("y2", label = "Cambio en movilidad (base 100)", valueRange = c(-50, 50), independentTicks = TRUE) %>%
  dySeries("mov_work", axis=('y2'))

y_rec <- x %>% mutate(rec = rollmean(retail_and_recreation_percent_change_from_baseline, 
                                        k = 14, fill = NA, align = "right"))

y_rec %>% ggplot(aes(x = date, y = rec))+
  geom_line()


mov_rec <- xts(x = y_rec$rec, order.by = y_rec$date)

mov_cases_rec <- cbind(casos_diarios, mov_rec)

dygraph(mov_cases_rec, main = "Movilidad por recreación y consumo vs casos Covid-19 confirmados en México")%>%
  dyAxis("y", label = "Casos diarios", valueRange = c(0, 20000.0), independentTicks = TRUE)%>%
  dyAxis("y2", label = "Cambio en movilidad (base 100)", valueRange = c(-60, 50), independentTicks = TRUE) %>%
  dySeries("mov_rec", axis=('y2'))


mov_cases <- cbind(casos_diarios, mov_rec, mov_work)

dygraph(mov_cases, main = "Movilidad por trabajo y recreación/consumo vs casos Covid-19 confirmados en México")%>%
  dyAxis("y", label = "Casos diarios", valueRange = c(0, 20000.0), independentTicks = TRUE)%>%
  dyAxis("y2", label = "Cambio en movilidad (base 100)", valueRange = c(-60, 50), independentTicks = TRUE) %>%
  dySeries("mov_rec", axis=('y2')) %>%
  dySeries("mov_work", axis=('y2'))


deaths <- xts(x = casos_mex$positivos_ma03, order.by = casos_mex$FECHA_INGRESO)
