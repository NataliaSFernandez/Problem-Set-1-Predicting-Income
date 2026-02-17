################################################################################
# PROBLEM SET 2: GENDER-LABOR Income Gap
# Script 05: Gender-Labor Income Gap (Section 2)
################################################################################
# OBJETIVO: Explorar si la relación edad-salario varia entre hombres y mujeres
#  y por cuanto se le puede atribuir esta diferencia a diferencias observables e 
# inexplicables
#
# INPUTS:  00_data/cleaned/data_cleaned.csv
# OUTPUTS: 
#   - 
################################################################################

# Limpiar entorno
rm(list = ls())

#Instalar librerías
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stargazer)) install.packages("stargazer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(boot)) install.packages("boot")

# Cargar librerías
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(boot)
})

cat("================================================================================\n")
cat("SECCIÓN 1: PERFIL EDAD-INGRESO\n")
cat("================================================================================\n")

#Log(w)=betha_1+betha_2 Femaile ´u
colnames(df)
