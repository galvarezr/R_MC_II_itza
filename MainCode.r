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

##### Base de datos ----
computoeinegi <- read.csv("2021 computoeinegi.csv")
str(computoeinegi)
head(computoeinegi)
# View(computoeinegi)

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

#Prueba Lupita
1+1

#prueba 2
3+3
