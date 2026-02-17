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


# ==============================================================================
# PASO 1: CARGAR DATOS
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 1: CARGAR DATOS LIMPIOS\n")
cat("================================================================================\n")

data1 <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data1), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data1)))

# ==============================================================================
# PASO 2: DEFINIR FUNCIONES 
# ==============================================================================

# 2.1 Función incondicional log(w)=beta_0+beta_1*Female+u

run_gender_model <- function(df) {
  model <- lm(log_income ~ female, data = df)
  return(model)
}

# 2.2 Gap condicional con controles con FWL

controls_A <- c("age", "age_squared", "totalHoursWorked", "factor(relab)", "factor(maxEducLevel)")

run_fwl_gender <- function(df,
                           y = "log_income",
                           g = "female",
                           controls = c("age", "age_squared", "totalHoursWorked", "factor(relab)", "factor(maxEducLevel)","estrato1")) {

  # quitar NAs en variables relevantes
  df2 <- df[, unique(c(y, g, "age", "age_squared", "totalHoursWorked", "relab"))]
  df2 <- na.omit(df2)

  # construir fórmulas
  controls_str <- paste(controls, collapse = " + ")
  f_g <- as.formula(paste(g, "~", controls_str))
  f_y <- as.formula(paste(y, "~", controls_str))

  # --- FWL ---
  g_res <- resid(lm(f_g, data = df2))  # female residualizada
  y_res <- resid(lm(f_y, data = df2))  # log_income residualizado

  # regresión final (residuales)
  fwl <- lm(y_res ~ g_res)

  # devolver coef y SE analítico (de esta regresión final)
  out <- list(
    n = nrow(df2),
    beta_female = coef(fwl)[["g_res"]],
    se_analytic = summary(fwl)$coefficients["g_res", "Std. Error"],
    t = summary(fwl)$coefficients["g_res", "t value"],
    p = summary(fwl)$coefficients["g_res", "Pr(>|t|)"],
    model = fwl
  )
  return(out)
}

# 2.3 Modelo FWL 
# 2.4 Bootstrap del gender coefficient
# 2.5 Perfiles edad-ingreso por género
# 2.6 Peak age por género

cat("\n================================================================================\n")
cat("PASO 3: EJECUCIÓN DE FUNCIONES\n")
cat("================================================================================\n")

model1 <- run_gender_model(data1)
summary(model1)