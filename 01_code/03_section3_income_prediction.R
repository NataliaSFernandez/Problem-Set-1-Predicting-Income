################################################################################
# PROBLEM SET 3: MODIFICAR TODA LA DESCRIPCIÓN 
# Section 2: Gender–Labor Income Gap
# Script 02: Gender_Labor_Income_Gap.R
################################################################################
# OBJETIVO:
# Analizar la brecha de ingresos laborales entre hombres y mujeres en Bogotá
# (GEIH 2018), evaluando:
#
# 1) La brecha incondicional (raw gender gap).
# 2) La brecha condicional controlando por edad, horas trabajadas,
#    tipo de vínculo laboral y nivel educativo.
# 3) La descomposición del coeficiente de género utilizando el teorema
#    Frisch–Waugh–Lovell (FWL).
# 4) La estimación de errores estándar tanto analíticos (OLS) como por
#    bootstrap.
# 5) La comparación de perfiles edad–ingreso predichos por género.
# 6) El cálculo de la edad de máximo ingreso (peak age) para hombres y
#    mujeres, junto con intervalos de confianza bootstrap.
#
# Este análisis permite distinguir entre la brecha salarial bruta y la
# brecha explicada por diferencias observables en características laborales
# y de capital humano, contribuyendo a la discusión sobre el principio de
# “equal pay for equal work”.
#
# INPUT:
#   - 00_data/cleaned/data_cleaned.csv
#
# OUTPUT:
#   - Tabla comparativa de brecha incondicional y condicional
#   - Tabla de edades pico (peak ages) por género
#   - Gráfico de perfiles edad–ingreso predichos
#   - Archivos exportados en:
#       02_output/tables/01_section2_gap
#       02_output/figures/01_section2_gap
#
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

cat("\n================================================================================\n") # nolint
cat("PASO 1: CARGAR DATOS LIMPIOS\n")
cat("================================================================================\n") # nolint

data <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data)))

cat("\n================================================================================\n") # nolint
cat("PASO 2:  DIVIDIR DATOS EN ENTRENAMIENTO Y PRUEBA\n")
cat("================================================================================\n") # nolint

#Dividir datos en entrenamiento (chunks 1-7) y validación (chunks 8-10)
train_df <- subset(data, chunk %in% 1:7)
valid_df <- subset(data, chunk %in% 8:10)

# Resumen de tamaños y chunks
cat("Train n:", nrow(train_df), " | chunks:", paste(sort(unique(train_df$chunk)), collapse = ","), "\n")
cat("Valid n:", nrow(valid_df), " | chunks:", paste(sort(unique(valid_df$chunk)), collapse = ","), "\n")

cat("\n================================================================================\n") # nolint
cat("PASO 3:  REESTIMACIÓN DE MODELOS\n")
cat("================================================================================\n") # nolint

#Modelo 1: Age + Age^2
#Modelo 2: Age
model2_train <- lm(log_income ~ age, data = train_df)
summary(model2_train) 

#Modelo 3: Gender gap incondicional 
model3_train <- lm(log_income ~ female, data = train_df)
summary(model3_train)

# Modelo 4: Gender gap condicional 
control_num <- c("age", "age_squared", "totalHoursWorked", "formal", "p6426")
control_fac <- c("maxEducLevel", "relab", "oficio", "sizeFirm") #Categorias
# categóricas como factor EN TRAIN
for(v in control_fac){
  train_df[[v]] <- factor(train_df[[v]])
}
# Alinear niveles en VALID con base en TRAIN (evita "new levels")
for(v in control_fac){
  valid_df[[v]] <- factor(valid_df[[v]], levels = levels(train_df[[v]]))
}
# Construir fórmula: numéricas + factor(categóricas)
rhs <- c(
  control_num,
  paste0("factor(", control_fac, ")")
)
f4 <- as.formula(paste("log_income ~", paste(rhs, collapse = " + ")))
# Estimar modelo 4 en TRAIN
model4_train <- lm(f4, data = train_df)
summary(model4_train)

cat("\n================================================================================\n") # nolint
cat("PASO 4:  PREDICCIÓN SET DE VALIDACIÓN\n")
cat("================================================================================\n") # nolint

#Modelo 3: Gender gap incondicional
valid_df$pred_model3 <- predict(model3_train, newdata = valid_df)

#Modelo 4: Gender gap condicional
valid_df$pred_model4 <- predict(model4_train, newdata = valid_df) 

# cuántas predicciones quedaron NA (niveles nuevos / missing en covariables)
n_na_pred4 <- sum(is.na(valid_df$pred_model4))
cat("Predicciones NA en Modelo 4:", n_na_pred4, "de", nrow(valid_df), "\n")

cat("\n================================================================================\n") # nolint
cat("PASO 5:  CALCULO RMSE\n")
cat("================================================================================\n") # nolint

#Modelo 3: Gender gap incondicional
rmse_model3 <- sqrt(mean((valid_df$log_income - valid_df$pred_model3)^2))

# Modelo 4 (na.rm=TRUE porque pueden existir NA en pred_model4)
rmse_model4 <- sqrt(mean((valid_df$log_income - valid_df$pred_model4)^2, na.rm = TRUE))

cat("RMSE Model 3 (Validation):", rmse_model3, "\n")
cat("RMSE Model 4 (Validation):", rmse_model4, "\n")
ok4 <- !is.na(valid_df$pred_model4) & !is.na(valid_df$log_income)
cat("Obs usadas en RMSE Modelo 4:", sum(ok4), "de", nrow(valid_df), "\n")