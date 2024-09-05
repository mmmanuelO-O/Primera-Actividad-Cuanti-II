#Instalar librerias
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(knitr)
library(kableExtra)
#Importar el dataset
Base_de_datos_Encuesta_CIEP_Abril_2020 <- read_dta("C:/Users/Dell/Downloads/Base de datos Encuesta CIEP Abril 2020.dta")
View(Base_de_datos_Encuesta_CIEP_Abril_2020)

########################
#Seleccionar las variables de interes por medio de Dplyr
