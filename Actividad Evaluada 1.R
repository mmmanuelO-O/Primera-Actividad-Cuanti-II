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
Variables_Filtradas

glimpse(Variables_Filtradas) #Para mostrar todas las columnas filtradas por edad y por sexo

# INTERPRETACION DE LOS DATOS OBTENIDOS EN LA PREGUNTA 4:
## En primer lugar, es importante establecer que, de 448 personas filtradas entre las edades de 18 y 35 años, 240 que respondieron esta encuesta fueron mujeres, mientras que 208 fueron hombres. 
## Esto demuestra una mayor disposición de las mujeres a participar en encuestas en comparación con los hombres. 
## En efecto, William G. Smith (2008) en su artículo titulado "Does Gender Influence Online Survey Participation?: A Record-linkage Analysis of University Faculty Online Survey Response Behavior", establece que las mujeres son más propensas a participar que los hombres en este tipo de instrumentos (p. 6).
### Con respecto al Nivel Educativo, al observar las tres medidas de tendencia central (media, mediana y moda), tanto en hombres como en mujeres notamos que los datos se acercan al número 5, es decir: "estudios universitarios incompletos". 
### Esto se puede deber al hecho de que una mayoría de las personas encuestadas se encuentran entre los 18 y 25 años de edad, período en el cual la mayoría de las personas jóvenes se encuentran realizando sus estudios universitarios. 
#### En cuanto a la Situación Laboral, en el caso de los hombres se observa que los datos se acercan al número 2, es decir que, en promedio, la mayoría trabaja al mismo tiempo que lleva a cabo sus estudios; mientras que en el caso de las mujeres, los datos se acercan al número 4, lo que significan que a la hora de haber realizado la encuesta se encuentraban desempleadas. 
#### Esto puede ser el resultado de la desvalorización de las mujeres en la fuerza laboral, que permea la mayoría de las empresas del país. 
##### Finalmente, en lo que concierne el Apoyo de las Parejas del Mismo Sexo, observamos que los hombres desaprueban, en mayor proporción, que las parejas del mismo sexo puedan tener el derecho a casarse, con un promedio de 5, utilizando una escala del 1 al 10, en la que 1 indica que desaprueba firmemente y el 10 que indica que aprueba firmemente; en comparación con las mujeres, con un promedio de 7. 
##### Esto puede estar relacionado a la mayor tolerancia por parte de las mujeres en lo que concierne al apoyo de las minorías, como las personas sexualmente diversas, mientras que los hombres, históricamente, han tenido un mayor reacio hacia esta comunidad. 

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




######################GRÁFICOS
#Nivel educativo por sexo
ggplot(DatosCIEP3, aes(x = NivelEducativo, fill = sexo)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Distribución del Nivel Educativo por Sexo", 
       x = "Nivel Educativo", 
       y = "Frecuencia") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()


#Este lo descrubrí hoy que le pedí a chat gpt que me enseñara nuevos entonces es más curiosidad pero es de densidad y se ve lindo
ggplot(DatosCIEP3, aes(x = NivelEducativo)) +
  geom_density(aes(fill = sexo), alpha = 0.5) +
  facet_wrap(~ sexo) +
  labs(title = "Densidad del Nivel Educativo por Sexo", x = "Nivel Educativo", y = "Densidad") +
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  theme_minimal()

# Nivel educativo combinado con situación laboral --> no estoy seguro si el gráfico está bueno :(

ggplot(edad_filtrada, aes(x = reorder(NivelEducativo,SituaciónLaboral),  y = SituaciónLaboral)) +
  geom_col(fill= "salmon") +
  labs(title = "Situación Laboral actual con respecto al Nivel Educativo",
       x = "Nivel Educativo",
       y = "Situación Laboral") +
  theme_bw()







# Instalar las librerias necesarias
library(haven)
library(kableExtra)
library(ggplot2)
library(knitr)
library(tidyverse)
library(dplyr)
# 1. DEscargue la base de datos del CIEP 2020 utilizada en el curso
DatosEncuestaCIEP_2020<- read_dta("C:/Users/Dell/Downloads/Base de datos Encuesta CIEP Abril 2020.dta")
View(Base_de_datos_Encuesta_CIEP_Abril_2020)
# 2. Filtre usando Dplyr la base de datos. con 5 variables que sean de su interes
DatosCIEP <- dplyr::select(DatosEncuestaCIEP_2020, sexo, edad, estudios, LABORA1, D6)

DatosCIEP$sexo <- as.numeric(DatosCIEP$sexo)
DatosCIEP$edad <- as.numeric(DatosCIEP$edad)
DatosCIEP$estudios <- as.numeric(DatosCIEP$estudios)
DatosCIEP <- DatosCIEP %>%
  rename(NivelEducativo = estudios)
DatosCIEP$LABORA1 <- as.numeric(DatosCIEP$LABORA1)
DatosCIEP <- DatosCIEP %>%
  rename(SituaciónLaboral = LABORA1 )
DatosCIEP$D6 <- as.numeric(DatosCIEP$D6)
DatosCIEP <- DatosCIEP %>%
  rename(Apoyo_ParejasMismoSexo = D6)

DatosCIEP <- na.omit(DatosCIEP)
# 3. Filtre usando Dplyr paraa analizar las variables solamente a las personas de  18 a 35 años
Edad_Filtrada <- DatosCIEP %>%
  dplyr::filter(edad >= 18 & edad <= 35)
Edad_Filtrada 
# 4. Brinde un resumen de las medidas de tendencia central de las variables seleccionadas en una sola tabla por sexo  e interpretelas
DatosCIEP$sexo <- factor(DatosCIEP$sexo, labels = c("Hombre", "Mujer"))

TCEdades_Filtradas <- DatosCIEP %>%
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

glimpse(TCEdades_Filtradas)
# 5. Elabore al menos dos gráficos distintos en ggplot con las variables seleccionadas
