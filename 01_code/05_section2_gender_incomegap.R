################################################################################
# PROBLEM SET 1: Predicting Income
# Section 2: Gender–Labor Income Gap
# Script 05: Gender_Labor_Income_Gap.R
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
#       02_output/tables/05_section2_gap
#       02_output/figures/05_section2_gap
#
################################################################################


# Limpiar entorno
rm(list = ls())

#Instalar librerías
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stargazer)) install.packages("stargazer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(boot)) install.packages("boot")
if (!require(gt)) install.packages("boot")

# Cargar librerías
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(boot)
  library(gt)
})

cat("\n================================================================================\n") # nolint
cat("PASO 1: CARGAR DATOS LIMPIOS\n")
cat("================================================================================\n") # nolint

data <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data)))

cat("\n================================================================================\n")
cat("PASO 2: REGRESIÓN INCONDICIONAL\n")
cat("================================================================================\n")

# Función incondicional log(w)=beta_0+beta_1*Female+u

model1 <- lm(log_income ~ female, data = data)
cat("\nModelo 1: log(ingreso) = β0 + β1*female + u\n")
cat(sprintf("  beta_1: %s\n",coef(model1)["female"] ,","))
cat(sprintf("  Observaciones: %s\n", format(nobs(model1), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model1)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model1)$adj.r.squared))

cat("\n================================================================================\n")
cat("PASO 3: REGRESIÓN CONDICIONAL SIN FWL\n")
cat("================================================================================\n") # nolint: line_length_linter.

# Gap condicional con controles 
# Se usa para verificar con el modelo con FWL 
model2<- lm(log_income ~ female +
              age + age_squared +
              totalHoursWorked +
              factor(relab) + #factor para variables categoricasd
              factor(maxEducLevel),
              data = data)

cat("\nModelo 2: log(ingreso) = β0 + β1*Female + β2*age + β3*Age2 + β4*Hours+ gamma(Relab)+ sigma(Educ)+u_i\n") 
cat(sprintf("  beta_1: %s\n",coef(model2)["female"] ,","))
cat(sprintf("  Observaciones: %s\n", format(nobs(model2), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model2)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model2)$adj.r.squared))

cat("\n================================================================================\n") # nolint: line_length_linter.
cat("PASO 4: REGRESIÓN FWL\n")
cat("================================================================================\n") # nolint

#  Gap condicional con controles con FWL

run_fwl_gender <- function(df,
                           y = "log_income",
                           g = "female",
                           controls = c("age", "age_squared", "totalHoursWorked", "factor(relab)", "factor(maxEducLevel)")) { # nolint: line_length_linter.


  # construir fórmulas de residualización
  controls_str <- paste(controls, collapse = " + ")
  f_g <- as.formula(paste(g, "~", controls_str)) # female ~ X
  f_y <- as.formula(paste(y, "~", controls_str)) # log_income ~ X

  # Paso 1: residualizar female y log_income respecto a X
  g_res <- resid(lm(f_g, data = df))  # female residualizada
  y_res <- resid(lm(f_y, data = df))  # log_income residualizado

   # Paso 2: regresión de residuales
  fwl <- lm(y_res ~ g_res)

  # Resultados principales
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

# Ejecutar FWL
res_fwl <- run_fwl_gender(data)
beta_hat <- res_fwl$beta_female
se_anal <- res_fwl$se_analytic

# Imprimir resultados
cat("\nModelo 3 (FWL): coeficiente de female controlando por X\n")
cat(sprintf("  N: %s\n", format(res_fwl$n, big.mark=",")))
cat(sprintf("  beta_female (FWL): %.4f\n", beta_hat))
cat(sprintf("  SE analítico: %.4f\n", se_anal))
cat(sprintf("  p-value: %.4g\n", res_fwl$p))

# Check: FWL debe coincidir con el modelo completo (model2)
cat("\nCheck FWL vs Modelo Condicional (OLS):\n")
cat(sprintf("  beta_female (OLS condicional): %.4f\n", coef(model2)["female"]))
cat(sprintf("  beta_female (FWL):            %.4f\n", beta_hat))
cat(sprintf("  Diferencia (debe ser ~0):     %.8f\n", coef(model2)["female"] - beta_hat)) # nolint


cat("\n================================================================================\n")
cat("PASO 5: BOOTSTRAP\n")
cat("================================================================================\n")

set.seed(123)

run_fwl_bootstrap <- function(df,
                              R = 500,
                              y = "log_income",
                              g = "female",
                              controls = c("age", "age_squared", "totalHoursWorked", # nolint
                                           "factor(relab)", "factor(maxEducLevel)")) { # nolint

  # 2) Fórmulas de residualización
  controls_str <- paste(controls, collapse = " + ") #hace un string unido por un +
  f_g <- as.formula(paste(g, "~", controls_str))
  f_y <- as.formula(paste(y, "~", controls_str))

  # 3) Estadístico bootstrap: beta_female vía FWL
  boot_fun <- function(d, idx) {
    dd <- d[idx, ]

    g_res <- resid(lm(f_g, data = dd))
    y_res <- resid(lm(f_y, data = dd))

    coef(lm(y_res ~ g_res))[["g_res"]]
  }

  boot::boot(df, statistic = boot_fun, R = R)
}

# Ejecutar bootstrap
boot_res <- run_fwl_bootstrap(data, R = 500)

# SE bootstrap
se_boot <- sd(boot_res$t)

# IC percentil
ci_boot <- boot::boot.ci(boot_res, type = c("perc", "basic"))

cat("\n--- Resultados Gender Gap (Condicional con FWL) ---\n")
cat(sprintf("N (muestra efectiva): %s\n", format(res_fwl$n, big.mark=",")))
cat(sprintf("beta_female (FWL): %.4f\n", beta_hat))
cat(sprintf("SE analítico (OLS): %.4f\n", se_anal))
cat(sprintf("SE bootstrap: %.4f\n", se_boot))


cat("\n================================================================================\n")
cat("PASO 6: TABLA COMPARATIVA\n")
cat("================================================================================\n")

# 1. Extraer coeficientes y SE
# ===============================

# Modelo 1 Incondicional
beta_uncond <- coef(model1)["female"]
se_uncond_anal <- summary(model1)$coefficients["female","Std. Error"]
r2_uncond <- summary(model1)$r.squared

# Modelo 2 condicional
# Beta y SE desde FWL
beta_cond <- res_fwl$beta_female
se_cond_anal <- res_fwl$se_analytic
n_cond <- res_fwl$n

# R² desde modelo completo
r2_cond <- summary(model2)$r.squared

# 2. Bootstrap incondicional
boot_uncond_fun <- function(d, idx){
  dd <- d[idx,]
  coef(lm(log_income ~ female, data = dd))["female"]
}

set.seed(123)
boot_uncond <- boot::boot(data, boot_uncond_fun, R = 500)
se_uncond_boot <- sd(boot_uncond$t)


# 3. Bootstrap condicional 

boot_cond_fun <- function(d, idx){
  dd <- d[idx, ]
  coef(lm(log_income ~ female +
            age + age_squared +
            totalHoursWorked +
            factor(relab) +
            factor(maxEducLevel),
          data = dd))["female"]
}

set.seed(123)
boot_cond <- boot::boot(data, boot_cond_fun, R = 500)
se_cond_boot <- sd(boot_cond$t)

# 4. Tabla final

results_table <- data.frame(
  Specification = c("Unconditional", "Conditional"),
  Beta_Female = c(beta_uncond, beta_cond),
  SE_Analytical = c(se_uncond_anal, se_cond_anal),
  SE_Bootstrap = c(se_uncond_boot, se_cond_boot),
  R_squared = c(r2_uncond, r2_cond)
)


stargazer(results_table,
          summary = FALSE,
          digits = 4,
          rownames = FALSE,
          title = "Gender Labor Income Gap",
          out = "02_output/tables/05_section2_gap/05_gender_gap_table.tex")


gt(results_table) %>%
  gtsave("02_output/tables/05_section2_gap/05_gender_gap_table.png")

cat("\nGuardado: 02_output/tables/05_section2_gap/05_gender_gap_table.tex\n")

cat("\n================================================================================\n") # nolint
cat("PASO 7: VISUALIZACIÓN PREDICTED AGE-LABOR INCOME PROFILES\n")
cat("================================================================================\n") # nolint

#revisar depronto hacer + graf con otra base relab y base educ
model_interact <- lm(log_income ~ female*(age + age_squared) +
                     totalHoursWorked +
                     factor(relab) +
                     factor(maxEducLevel),
                     data = data)

age_seq <- seq(min(data$age, na.rm=TRUE),
               max(data$age, na.rm=TRUE),
               by = 1)

mean_hours <- mean(data$totalHoursWorked, na.rm=TRUE)

# Categoría base
base_relab <- levels(factor(data$relab))[1] #REVISAR
base_educ  <- levels(factor(data$maxEducLevel))[1]  #REVISAR

# Crear grid
pred_data <- expand.grid(
  age = age_seq,
  female = c(0, 1)
)

pred_data$age_squared <- pred_data$age^2
pred_data$totalHoursWorked <- mean_hours
pred_data$relab <- factor(base_relab, levels = levels(factor(data$relab)))
pred_data$maxEducLevel <- factor(base_educ, levels = levels(factor(data$maxEducLevel)))

# Predicción en log (y forzar numérico)
pred_data$pred_log <- as.numeric(predict(model_interact, newdata = pred_data))
pred_data$pred_income <- exp(pred_data$pred_log)

# Etiquetas
pred_data$gender <- factor(pred_data$female,
                           levels = c(0, 1),
                           labels = c("Male", "Female"))

# Máximos por grupo
peaks <- pred_data %>%
  group_by(gender) %>%
  slice_max(order_by = pred_income, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot
p <- ggplot(pred_data, aes(age, pred_income, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = peaks, size = 3) +
  geom_text(
    data = peaks,
    aes(x = age, y = label_y,
        label = paste0("Peak: age ", age, "\n", format(round(pred_income,0), big.mark=","))),
    show.legend = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  theme_bw()

print(p)
# Guardar

ggsave("02_output/figures/05_section2_gap/05_age_labor_income_profiles.png",
       plot = p, width = 10, height = 6, dpi = 300)

cat("\nGuardado: 02_output/figures/05_section2_gap/05_age_labor_income_profiles.png\n")

cat("\n================================================================================\n")
cat("PASO 9: IMPLIED PEAK AGES \n")
cat("================================================================================\n")

coefs <- coef(model_interact)

b_age <- coefs["age"]
b_age2 <- coefs["age_squared"]

b_age_f <- coefs["female:age"]
b_age2_f <- coefs["female:age_squared"]

#Despues de derivar log(w)/Age
peak_male <- -b_age / (2*b_age2)
peak_female <- -(b_age + b_age_f) /
               (2*(b_age2 + b_age2_f))

cat("\nPeak age (Male):", peak_male)
cat("\nPeak age (Female):", peak_female)

boot_peak_fun <- function(d, idx){
  dd <- d[idx,]

#bootstrap intervalos de confianza
  m <- lm(log_income ~ female*(age + age_squared) +
          totalHoursWorked +
          factor(relab) +
          factor(maxEducLevel),
          data = dd)

  cfs <- coef(m)

  b_age <- cfs["age"]
  b_age2 <- cfs["age_squared"]

  b_age_f <- cfs["female:age"]
  b_age2_f <- cfs["female:age_squared"]

  peak_m <- -b_age / (2*b_age2)
  peak_f <- -(b_age + b_age_f) /(2*(b_age2 + b_age2_f))

  return(c(peak_m, peak_f))
}

boot_peaks <- boot::boot(data, boot_peak_fun, R=500)

boot::boot.ci(boot_peaks, index=1, type="perc") # male
boot::boot.ci(boot_peaks, index=2, type="perc") # female

# SE bootstrap
se_peak_male <- sd(boot_peaks$t[,1])
se_peak_female <- sd(boot_peaks$t[,2])

# Intervalos percentil
ci_male <- boot::boot.ci(boot_peaks, index=1, type="perc")
ci_female <- boot::boot.ci(boot_peaks, index=2, type="perc")

ci_male_lower <- ci_male$percent[4]
ci_male_upper <- ci_male$percent[5]

ci_female_lower <- ci_female$percent[4]
ci_female_upper <- ci_female$percent[5]

peak_table <- data.frame(
  Group = c("Male", "Female"),
  Peak_Age = c(peak_male, peak_female),
  SE_Bootstrap = c(se_peak_male, se_peak_female),
  CI_Lower = c(ci_male_lower, ci_female_lower),
  CI_Upper = c(ci_male_upper, ci_female_upper)
)

stargazer(peak_table,
          summary = FALSE,
          digits = 2,
          rownames = FALSE,
          title = "Implied Peak Ages by Gender",
          out = "02_output/tables/05_section2_gap/05_peak_ages_section2.tex")


ggsave("02_output/figures/05_section2_gap/05_peak_ages_section2.png",
       plot = p, width = 10, height = 6, dpi = 300)

cat("\nGuardado: 02_output/figures/05_section2_gap/05_peak_ages_section2.png\n")