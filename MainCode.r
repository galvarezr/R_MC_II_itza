## Trabajo final MAPP Otoño. Métodos cuantitativos.
## Integrantes:
## Guadalupe Álvarez
## Ángel Carrillo
## Eduardo Muñiz

Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica
rm(list=ls()) #Limpia todas las variables y objetos creados

##### Paquetería ----
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)

library(cluster)
library(dplyr)
library(pacman)
library(factoextra)
library(dendextend)
library(purrr)

#PAQUETERÍA PARA ANOVA Y REGRESIÓN LINEAL
library(lme4)
library(lmerTest)


##### Base de datos ----
computoeinegi <- read.csv("2021 computoeinegi.csv")
str(computoeinegi)
head(computoeinegi)
# View(computoeinegi)

###################################
####### Indice de cosas por hacer:
###################################

# FECHA DE ENTREGA: 13 Y 14 DE DICIEMBRE

# Limpiar la base
# Estadistica descriptiva LALO
# ANOVA LUPITA
# Regresión lineal con variables explicativas CARILLO
# Hito 1: DEBE ESTAR LISTO EL  25-NOV EN LA NOCHE
# Ahí cordinarnos por mensajería sobre lo siguiente.

# Usar variables explicativas para analisis de componentes principales
# crear subconjunto de variables
# utilizar estas variables explicativas para:
# Conglomerados
# Nuevo grupos
# con nuevos grupos analisis jerárquico. 

##### Estadística descriptiva ----

gr_participacion <- ggplot( computoeinegi, 
                   aes(x=distrito , y=participacion) )+ 
              geom_point() + 
              labs(title="Participación electoral", 
                  subtitle="Proceso Electoral Federal 2021.", 
                  x="Distrito electoral federal",
                  y="Porcentaje de participación", 
                  caption="Elaboración propia con datos de INE(2021).") +
              theme_economist()+
              theme(axis.text.x=element_text(angle=50,size=12))

plot(gr_participacion)

########
      ##  ANOVA  ##

#Creamos una base con la variable categórica complejidad que nos indicará el número de grupos
# y la variable de interés será el porcentaje de participación

Anova_part <- computoeinegi [, -c(1,2,3,4,5,6,8,9,11,12,13,14,15,16,17)]

# Gráfica simple
ggplot(data = Anova_part, aes(x = 1:nrow(Anova_part), y = participacion)) +
  geom_point() +
  theme_bw() 

# Gráfica por complejidad
ggplot(data = Anova_part, aes(x = 1:nrow(Anova_part), 
                              y = participacion, colour = complejidad)) +
                              geom_point() +
                              theme_bw() 

# Grafica de la gran media
ggplot(data = Anova_part, aes(x = 1:nrow(Anova_part), y = participacion, colour = complejidad)) +
  geom_point() +
  geom_hline(yintercept = mean(Anova_part$participacion), colour = "purple", size = 1) +
  theme_bw()

# Graficar la gran media y las medias de los grupos
ggplot(data = Anova_part, aes(x = 1:nrow(Anova_part), y = participacion, colour = complejidad)) +
  geom_point() +
  geom_hline(yintercept = mean(Anova_part$participacion), colour = "purple", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="	
Altamente Concentrado 1"]), 
             colour = "orangered1", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="	
Altamente Concentrado 2"]), 
             colour = "orange", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Concentración Media"]), 
             colour = "olivedrab", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Concentrado 1"]), 
             colour = "green", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Concentrado 2"]), 
             colour = "turquoise2", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Disperso 1"]), 
             colour = "blue", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Disperso 2"]), 
             colour = "royalblue", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Muy Disperso 1"]), 
             colour = "violet", size = 1) +
  geom_hline(yintercept = mean(Anova_part$participacion[Anova_part$complejidad=="Muy Disperso 2"]), 
             colour = "deeppink", size = 1) +
  theme_bw()

###
### ANOVA 
### Hipótesis Nula = las medias son iguales
### Hipótesis Alternativa = No todas las medias son iguales

anova(lm(participacion ~ complejidad, data = Anova_part))


