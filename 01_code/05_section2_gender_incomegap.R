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

cat("\n================================================================================\n") # nolint
cat("PASO 1: CARGAR DATOS LIMPIOS\n")
cat("================================================================================\n") # nolint

data <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data)))

# ==============================================================================
# PASO 2: REGRESIÓN INCONDICIONAL 
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 2: REGRESIÓN INCONDICIONAL\n")
cat("================================================================================\n")

# Función incondicional log(w)=beta_0+beta_1*Female+u

model1 <- lm(log_income ~ female, data = data)
cat("\nModelo 1: log(ingreso) = β0 + β1*female + u\n")
cat(sprintf("  Observaciones: %s\n", format(nobs(model1), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model1)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model1)$adj.r.squared))

cat("\n================================================================================\n")
cat("PASO 3: REGRESIÓN CONDICIONAL\n")
cat("================================================================================\n")

# Gap condicional con controles 
model2<- lm(log_income ~ female +
              age + age_squared +
              totalHoursWorked +
              factor(relab) +
              factor(maxEducLevel),
              data = data)

cat("\nModelo 2: log(ingreso) = β0 + β1*age + β2*age²\n")
cat(sprintf("  Observaciones: %s\n", format(nobs(model2), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model2)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model2)$adj.r.squared))

cat("\n================================================================================\n")
cat("PASO 4: REGRESIÓN FWL\n")
cat("================================================================================\n")

#  Gap condicional con controles con FWL

run_fwl_gender <- function(df,
                           y = "log_income",
                           g = "female",
                           controls = c("age", "age_squared", "totalHoursWorked", "factor(relab)", "factor(maxEducLevel)")) { # nolint: line_length_linter.


  # construir fórmulas
  controls_str <- paste(controls, collapse = " + ")
  f_g <- as.formula(paste(g, "~", controls_str))
  f_y <- as.formula(paste(y, "~", controls_str))

  # --- FWL ---
  g_res <- resid(lm(f_g, data = df))  # female residualizada
  y_res <- resid(lm(f_y, data = df))  # log_income residualizado

  # regresión final (residuales)
  fwl <- lm(y_res ~ g_res)

  # devolver coef y SE analítico 
  out <- list(
    n = nrow(df),
    beta_female = coef(fwl)[["g_res"]],
    se_analytic = summary(fwl)$coefficients["g_res", "Std. Error"],
    t = summary(fwl)$coefficients["g_res", "t value"],
    p = summary(fwl)$coefficients["g_res", "Pr(>|t|)"],
    model = fwl
  )
  return(out)
}

# 4 Gap condicional con controles con FWL 
res_fwl <- run_fwl_gender(data)

beta_hat <- res_fwl$beta_female
se_anal <- res_fwl$se_analytic


cat("\n================================================================================\n")
cat("PASO 5: BOOTSTRAP\n")
cat("================================================================================\n")


# 2.4 Bootstrap del gender coefficient
set.seed(123)
run_fwl_bootstrap <- function(df,
                              R = 500,
                              y = "log_income",
                              g = "female",
                              controls = c("age", "age_squared", "totalHoursWorked", # nolint
                                           "factor(relab)", "factor(maxEducLevel)")) { # nolint


  controls_str <- paste(controls, collapse = " + ")
  f_g <- as.formula(paste(g, "~", controls_str))
  f_y <- as.formula(paste(y, "~", controls_str))

  boot_fun <- function(d, idx) {
    dd <- d[idx, ]

    g_res <- resid(lm(f_g, data = dd))
    y_res <- resid(lm(f_y, data = dd))

    coef(lm(y_res ~ g_res))[["g_res"]]
  }

  boot::boot(df, statistic = boot_fun, R = R)
}

#2.4 Bootstrap 
boot_res <- run_fwl_bootstrap(data, R = 500)
se_boot <- sd(boot_res$t)
ci_boot <- boot::boot.ci(boot_res, type = c("perc", "basic"))

cat("\n--- Resultados Gender Gap (Condicional con FWL) ---\n")
cat(sprintf("beta_female: %.4f\n", beta_hat))
cat(sprintf("SE analítico (OLS): %.4f\n", se_anal))
cat(sprintf("SE bootstrap (sd betas): %.4f\n", se_boot))
print(ci_boot)

# 2.5 Perfiles edad-ingreso por género
# 2.6 Peak age por género