## 

library(sf)
library(ggplot2)
library(tidyverse)
library(treemap)
library(reshape2)
library(zoo)

rm(list=ls())

## Abre BD SINAVE
fecha <- paste0(format(Sys.Date(), "%y"), format(Sys.Date(), "%m"), format(Sys.Date()-1, "%d"))
load(paste0("data/", fecha, "sinave.Rda"))

lapply(sinave, class)

# Genera DF de resultados
res <- as.data.frame(cbind(CLASIFICACION_FINAL = c(1, 2, 3, 4, 5, 6, 7), 
                                 RESULTADO = c("Positivo", "Positivo", "Positivo",
                                        "Sospechoso", "Sospechoso", "Sospechoso",
                                        "Negativo")))

# Transformaciones a la BD
entidades <- read_csv(file = "data/ENTIDADES.csv")              # Importar catalogo de entidades
df <- inner_join(sinave, res, by = "CLASIFICACION_FINAL")       # Union con resultado final
df <- df %>% 
  left_join(entidades, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))  # Unión con entidades de atención
df <- data.table::setnames(df, "ENTIDAD_FEDERATIVA", "ENTIDAD_UM_NOM")

df <- df %>% 
  left_join(entidades, by = c("ENTIDAD_RES" = "CLAVE_ENTIDAD"))  # Unión con entidades de residencia
df <- data.table::setnames(df, "ENTIDAD_FEDERATIVA", "ENTIDAD_RES_NOM")

df <- df %>% 
  mutate(RESULTADO_FINAL = ifelse(is.na(FECHA_DEF), "", "Fallecido"))

### Plots de Entidades
df %>%
  filter(RESULTADO == "Positivo") %>%
  group_by(RESULTADO_FINAL, ENTIDAD_UM_NOM) %>%
  summarize(total = n()) %>%
  ggplot() +
  geom_col(aes(x = reorder(ENTIDAD_UM_NOM, total, sum), y = total, fill = RESULTADO_FINAL)) +
  labs(title = "Casos totales atendidos en las Unidades Médicas", x = "Localización de UM", 
       y = "Casos totales", fill = "Resultado") +
  scale_fill_discrete(name = "Resultado", labels = c("Recuperados", "Fallecidos")) +
  scale_y_continuous(breaks = seq(0,800000,100000), labels = scales::comma) +
  coord_flip() +
  theme_bw()

df %>%
  filter(RESULTADO == "Positivo") %>%
  group_by(RESULTADO_FINAL, ENTIDAD_RES_NOM) %>%
  summarize(total = n()) %>%
  ggplot() +
  geom_col(aes(x = reorder(ENTIDAD_RES_NOM, total, sum), y = total, fill = RESULTADO_FINAL)) +
  labs(title = "Lugar de residencia de contagiados", x = "Lugar de Residencia", 
       y = "Casos totales", fill = "Resultado") +
  scale_fill_discrete(name = "Resultado", labels = c("Recuperados", "Fallecidos")) +
  scale_y_continuous(breaks = seq(0,800000,100000), labels = scales::comma) +
  coord_flip() +
  theme_bw()

# Resumen de fallecidos
df %>% filter(RESULTADO_FINAL == "Fallecido") %>%
  group_by(RESULTADO) %>%
  summarize(total = n())

### Plots de sexo
df %>%
  filter(RESULTADO == "Positivo") %>%
  group_by(SEXO, RESULTADO_FINAL) %>%
  summarize(total = n()) %>%
  ggplot() +
  geom_col(aes(x = SEXO, y = total, fill = RESULTADO_FINAL))

df %>%
  filter(RESULTADO == "Positivo") %>%
  group_by(SEXO, RESULTADO_FINAL, EDAD) %>%
  #summarize(total = n()) %>%
  ggplot() +
  geom_boxplot(aes(x = SEXO, y = EDAD, fill = RESULTADO_FINAL)) +
  labs(title = "Edad de mujeres y hombres por resultado", x = "Sexo", 
       y = "Edad", fill = "Resultado final")+
  scale_fill_discrete(name = "Resultado final", labels = c("Recuperados", "Fallecidos")) +
  scale_x_discrete(labels = c("Mujer", "Hombre")) +
  theme_bw()

df %>%
  filter(RESULTADO == "Positivo") %>%
  group_by(SEXO, RESULTADO_FINAL, EDAD) %>%
  #summarize(total = n()) %>%
  ggplot() +
  geom_boxplot(aes(x = RESULTADO_FINAL, y = EDAD, fill = SEXO)) +
  labs(title = "Edad de recuperados y fallecidos por sexo", x = "Resultado final", 
       y = "Edad", fill = "Sexo")+
  scale_fill_discrete(name = "Sexo", labels = c("Mujer", "Hombre")) +
  scale_x_discrete(labels = c("Recuperados", "Fallecidos")) +
  theme_bw()

### Densidades de edad
labels <- c("Mujer", "Hombre")
names(labels) <- c("1", "2")

df %>%
  filter(RESULTADO == "Positivo") %>%
  group_by(SEXO, RESULTADO_FINAL, EDAD) %>%
  #summarize(total = n()) %>%
  ggplot() +
  geom_density(aes(x = EDAD, color = RESULTADO_FINAL)) +
  facet_grid(SEXO~., labeller = labeller(SEXO = labels)) +
  labs(title = "Densidad de casos por edad y sexo", x = "Edad", 
       y = "Densidad", color = "Resultado final") +
  scale_color_discrete(name = "Resultado final", labels = c("Recuperado", "Fallecido")) +
  theme_bw()

### Series de tiempo
tipo_pac <- df %>%
  #filter(CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 2 | CLASIFICACION_FINAL == 3) %>%
  group_by(FECHA_INGRESO, TIPO_PACIENTE, RESULTADO, RESULTADO_FINAL) %>%
  summarize(total = n()) %>%
  mutate(ma01 = rollmean(total, k = 2, fill = NA, align = "right"),
         ma02 = rollmean(total, k = 7, fill = NA, align = "right"),
         ma03 = rollmean(total, k = 14, fill = NA, align = "right"))

ggplot(tipo_pac) +
  geom_line(aes(x = FECHA_INGRESO, y = ma01, color = TIPO_PACIENTE)) +
  facet_grid(RESULTADO ~ RESULTADO_FINAL)





# Load package
library(networkD3)

links_um <- df %>%
  filter(RESULTADO_FINAL == "Fallecido") %>%
  group_by(RESULTADO) %>%
  select("ENTIDAD_RES_NOM", "ENTIDAD_UM_NOM") %>%
  group_by(ENTIDAD_RES_NOM, ENTIDAD_UM_NOM) %>%
  summarise(total = n()) %>%
 # mutate(Intrazonal = ifelse(ENTIDAD_RES_NOM == ENTIDAD_UM_NOM, "Intrazonal", "Ok")) %>%
 # filter(Intrazonal != "Intrazonal") %>%
  filter(total > 50)

colnames(links_um) <- c("source", "target", "value")#, "Intrazonal")  

nodes_um <- data.frame(name=c(as.character(links_um$source), as.character(links_um$target)) %>% unique())

links_um$IDsource = match(links_um$source, nodes_um$name) - 1 
links_um$IDtarget = match(links_um$target, nodes_um$name) - 1 

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

sankeyNetwork(Links = links_um, Nodes = nodes_um, 
              Source = "IDsource",
              Target = "IDtarget", 
              Value = "value", 
              NodeID = "name",
              units = "casos", 
              nodeWidth=10, fontSize=13, nodePadding=20, sinksRight=FALSE, colourScale=ColourScal)

df %>%
  group_by(RESULTADO) %>%
  summarize(total = n())


