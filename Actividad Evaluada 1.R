#Instalar librerias
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)
#Importar el dataset
Base_de_datos_Encuesta_CIEP_Abril_2020 <- read_dta("C:/Users/Dell/Downloads/Base de datos Encuesta CIEP Abril 2020.dta")
View(Base_de_datos_Encuesta_CIEP_Abril_2020)

########################
#Seleccionar las variables de interes por medio de Dplyr

# Ejercicio Clase 2 - Posibilidad 

# 1. seleccione de la base de datos 3 variables que considere de su interes y que considera que estan relacionadas 
# limpieza de datos
## seleccione las variables de su interes
datos_CIEP_2020_2 <- dplyr::select(datos_CIEP_2020, notaPJ, notaAL, notaGOB)

datos_CIEP_2020_2$notaPJ <- as.numeric(datos_CIEP_2020_2$notaPJ)
datos_CIEP_2020_2$notaAL <- as.numeric(datos_CIEP_2020_2$notaAL)
datos_CIEP_2020_2$notaGOB <- as.numeric(datos_CIEP_2020_2$notaGOB)

# eliminar filas con valores NA 
datos_CIEP_2020_2 <- na.omit(datos_CIEP_2020_2)

# 2. calcule las medidas de tendencia central y de dispersion de cada una 
# calculo de la medida de tendencia central de la primera variable (nota poder judicial)
summary(datos_CIEP_2020_2$notaPJ)
table(datos_CIEP_2020_2$notaPJ)
mean(datos_CIEP_2020_2$notaPJ)

# calculo de la medida de tendencia central de la segunda variable (nota asamblea legislativa)
summary(datos_CIEP_2020_2$notaAL)
table(datos_CIEP_2020_2$notaAL)
mean(datos_CIEP_2020_2$notaAL)

# calculo de la medida de tendencia central de la tercera variable (nota gobierno)
summary(datos_CIEP_2020_2$notaGOB)
table(datos_CIEP_2020_2$notaGOB)
mean(datos_CIEP_2020_2$notaGOB)

## calculo de la medida de dispersion de la primera variable (nota poder judicial)
sd(datos_CIEP_2020_2$notaPJ)
var(datos_CIEP_2020_2$notaPJ)
IQR(datos_CIEP_2020_2$notaPJ) 

## calculo de la medida de dispersion de la segunda variable (nota asamblea legislativa)
sd(datos_CIEP_2020_2$notaAL)
var(datos_CIEP_2020_2$notaAL)
IQR(datos_CIEP_2020_2$notaAL)

## calculo de la medida de dispersion de la tercera variable (nota gobierno)
sd(datos_CIEP_2020_2$notaGOB)
var(datos_CIEP_2020_2$notaGOB)
IQR(datos_CIEP_2020_2$notaGOB)

# 3. grafiquelas de manera unificada 
# visualización de la distribución de la nota del Poder Judicial 
## diagrama de caja 
ggplot(datos_CIEP_2020_2, aes(x = notaPJ)) + 
  geom_boxplot() + 
  labs(x = "nota_PJ") +
  theme_bw()

### histograma con densidad 
ggplot(datos_CIEP_2020_2, aes(x = notaPJ)) +
  geom_histogram(fill = "lightblue") +
  labs(x = "nota_PJ")+
  theme_bw()

# visualización de la distribución de la nota de la Asamblea Legislativa 
## diagrama de caja 
ggplot(datos_CIEP_2020_2, aes(x = notaAL)) + 
  geom_boxplot() + 
  labs(x = "nota_AL") +
  theme_bw()

### histograma con densidad 
ggplot(datos_CIEP_2020_2, aes(x = notaAL)) +
  geom_histogram(fill = "lightpink") +
  labs(x = "nota_AL")+
  theme_bw()

# visualización de la distribución de la nota del Gobierno 
## diagrama de caja 
ggplot(datos_CIEP_2020_2, aes(x = notaGOB)) + 
  geom_boxplot() + 
  labs(x = "nota_GOB") +
  theme_bw()

### histograma con densidad 
ggplot(datos_CIEP_2020_2, aes(x = notaGOB)) +
  geom_histogram(fill = "salmon") +
  labs(x = "nota_GOB")+
  theme_bw()

# calcular los promedios de las variables 
promedios <- colMeans(datos_CIEP_2020_2)
promedios

# crear un marco de datos para el gráfico 
datos_grafico <- data.frame(variable = names(promedios), promedio = promedios)

# etiquetar las variables correctamente 
etiquetas <- c("Poder Judicial", "Asamblea Legislativa", "Gobierno")
datos_grafico$variable <- factor(datos_grafico$variable, levels = names(promedios), labels = etiquetas)

library(ggplot2)

# graficar los promedios 
ggplot(datos_grafico, aes(x = reorder(variable, promedio), y = promedio)) + 
  geom_bar(stat = "identity", fill = "lightblue") + 
  labs(title = "Promedio de notas por categoría", 
       x = "Categoría", 
       y = "Promedio") +
  theme_bw() +
  coord_flip()
################################################################################################################################################################################################
################################################ACTIVIDAD EVALUADA################################################
#Descargar librerias necesarias
library(haven)
library(kableExtra)
library(ggplot2)
library(knitr)
library(tidyverse)
library(dplyr)
#Descarge la base de datos del CIEP 2020 utilzada en el curso
Base_de_datos_Encuesta_CIEP_Abril_2020 <- read_dta("C:/Users/Dell/Downloads/Base de datos Encuesta CIEP Abril 2020.dta")
View(Base_de_datos_Encuesta_CIEP_Abril_2020)
#Filtre usando Dplyr la base de datos con 5 variables que sean de su interés. 
DatosCIEP3 <- dplyr::select(Base_de_datos_Encuesta_CIEP_Abril_2020, sexo, edad, estudios, LABORA1, D6)

DatosCIEP3$sexo <- as.numeric(DatosCIEP3$sexo)
DatosCIEP3$edad <- as.numeric(DatosCIEP3$edad)
DatosCIEP3$estudios <- as.numeric(DatosCIEP3$estudios)
DatosCIEP3 <- DatosCIEP3 %>%
  rename(NivelEducativo = estudios)
DatosCIEP3$LABORA1 <- as.numeric(DatosCIEP3$LABORA1)
DatosCIEP3 <- DatosCIEP3 %>%
  rename(SituaciónLaboral = LABORA1 )
DatosCIEP3$D6 <- as.numeric(DatosCIEP3$D6)
DatosCIEP3 <- DatosCIEP3 %>%
  rename(Apoyo_ParejasMismoSexo = D6)

DatosCIEP3 <- na.omit(DatosCIEP3)
##################################################### CREO QUE HICE 3 Y 4, solo que no se como hacer la moda, mas que con table, podría intentar hacer un data frame pero no estoy seguro
table(DatosCIEP3$sexo)
DatosCIEP3$sexo <- factor(DatosCIEP3$sexo, labels = c("Hombre", "Mujer"))

Variables_Filtradas <- DatosCIEP3 %>%
  dplyr::filter(edad >= 15 & edad <= 35) %>%
  select(NivelEducativo, SituaciónLaboral, Apoyo_ParejasMismoSexo, sexo) %>%
  group_by(sexo) %>%
  summarise(
    MediaNivelEducativo = mean(NivelEducativo),
    MedianaNivelEducativo = median(NivelEducativo),
    MediaSituaciónLaboral = mean(SituaciónLaboral),
    MedianaSituaciónLaboral = median(SituaciónLaboral),
    MediaApoyoParejas = mean(Apoyo_ParejasMismoSexo),
    MedianaApoyoParejas = median(Apoyo_ParejasMismoSexo)
  )
Variables_Filtradas

# PREGUNTA 3, SEGUNDA POSIBILIDAD: filtre usando dplyr para analizar solamente a las personas de 18 a 35 años
edad_filtrada <- DatosCIEP3 %>%
  dplyr::filter(edad >= 18 & edad <= 35)
edad_filtrada 

# PREGUNTA 4, SEGUNDA POSIBILIDAD: Brinde un resumen de las medidas de tendencia de las variables seleccionadas en una sola tabla por sexo e interpretelas.
edades_filtradas <- DatosCIEP3 %>%
  dplyr::filter(edad >= 18 & edad <= 35) %>%
  select(NivelEducativo, SituaciónLaboral, Apoyo_ParejasMismoSexo, sexo) %>%
  group_by(sexo) %>%
  summarise(
    MediaNivelEducativo = mean(NivelEducativo),
    MedianaNivelEducativo = median(NivelEducativo),
    ModaNivelEducativo = as.numeric(names(sort(table(NivelEducativo), decreasing = TRUE))[1]), 
    MediaSituaciónLaboral = mean(SituaciónLaboral),
    MedianaSituaciónLaboral = median(SituaciónLaboral),
    ModaSituaciónLaboral = as.numeric(names(sort(table(SituaciónLaboral), decreasing = TRUE))[1]),
    MediaApoyoParejas = mean(Apoyo_ParejasMismoSexo),
    MedianaApoyoParejas = median(Apoyo_ParejasMismoSexo), 
    ModaApoyoParejas = as.numeric(names(sort(table(Apoyo_ParejasMismoSexo), decreasing = TRUE))[1]),
  )
edades_filtradas
