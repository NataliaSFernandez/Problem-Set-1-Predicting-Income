################################################################################
# PROBLEM SET 1: PREDICTING INCOME
# Script 04: Age-Income Profile (Section 1 - Imputed Version)
################################################################################
# OBJETIVO: Estimar perfil edad-ingreso con datos imputados y análisis de robustez
#
# INPUTS:  00_data/cleaned/data_cleaned_imputed.csv
# OUTPUTS: 
#   - 02_output/tables/age_income_regressions_imputed.tex
#   - 02_output/figures/age_income_profile_imputed.png
#   - 02_output/tables/peak_age_estimates_imputed.tex
################################################################################

# Limpiar entorno
rm(list = ls())

# Cargar librerías
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(boot)
})

cat("================================================================================\n")
cat("SECCIÓN 1: PERFIL EDAD-INGRESO (IMPUTED VERSION)\n")
cat("================================================================================\n")

# ==============================================================================
# PASO 1: CARGAR DATOS
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 1: CARGAR DATOS LIMPIOS (VERSIÓN IMPUTADA)\n")
cat("================================================================================\n")

data <- read.csv("00_data/cleaned/data_cleaned_imputed.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data)))
cat(sprintf("  Ingresos imputados: %s (%.1f%%)\n", 
            format(sum(data$income_imputed), big.mark=","),
            mean(data$income_imputed)*100))

# ==============================================================================
# PASO 2: REGRESIÓN INCONDICIONAL (Solo edad)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 2: REGRESIÓN INCONDICIONAL\n")
cat("================================================================================\n")

# Modelo 1: log(w) ~ age + age²
model1 <- lm(log_income ~ age + age_squared, data = data)

cat("\nModelo 1: log(ingreso) = β0 + β1*age + β2*age²\n")
cat(sprintf("  Observaciones: %s\n", format(nobs(model1), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model1)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model1)$adj.r.squared))

# Calcular edad de pico
beta1 <- coef(model1)["age"]
beta2 <- coef(model1)["age_squared"]
peak_age_m1 <- -beta1 / (2 * beta2)

cat(sprintf("\nEdad de pico de ingresos (Modelo 1): %.1f años\n", peak_age_m1))

# ==============================================================================
# PASO 3: REGRESIÓN CONDICIONAL (Con controles)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 3: REGRESIÓN CONDICIONAL\n")
cat("================================================================================\n")

# Modelo 2: log(w) ~ age + age² + totalHoursWorked + factor(relab)
model2 <- lm(log_income ~ age + age_squared + totalHoursWorked + factor(relab), 
             data = data)

cat("\nModelo 2: log(ingreso) = β0 + β1*age + β2*age² + β3*horas + γ*relab\n")
cat(sprintf("  Observaciones: %s\n", format(nobs(model2), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model2)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model2)$adj.r.squared))

# Calcular edad de pico
beta1_m2 <- coef(model2)["age"]
beta2_m2 <- coef(model2)["age_squared"]
peak_age_m2 <- -beta1_m2 / (2 * beta2_m2)

cat(sprintf("\nEdad de pico de ingresos (Modelo 2): %.1f años\n", peak_age_m2))

# ==============================================================================
# PASO 4: ANÁLISIS DE ROBUSTEZ (Solo ingresos observados)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 4: ANÁLISIS DE ROBUSTEZ - SOLO INGRESOS OBSERVADOS\n")
cat("================================================================================\n")

# Filtrar solo observaciones con ingreso observado (no imputado)
data_observed <- data[data$income_imputed == 0, ]

cat(sprintf("\nSubmuestra de ingresos observados: %s obs\n", format(nrow(data_observed), big.mark=",")))

# Modelo 1 - solo observados
model1_obs <- lm(log_income ~ age + age_squared, data = data_observed)
beta1_obs <- coef(model1_obs)["age"]
beta2_obs <- coef(model1_obs)["age_squared"]
peak_age_obs <- -beta1_obs / (2 * beta2_obs)

cat(sprintf("Edad de pico (solo observados): %.1f años\n", peak_age_obs))
cat(sprintf("Diferencia vs muestra completa: %.1f años\n", peak_age_m1 - peak_age_obs))

# ==============================================================================
# PASO 5: TABLA DE REGRESIONES
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 5: GENERAR TABLA DE REGRESIONES\n")
cat("================================================================================\n")

# Crear directorio si no existe
dir.create("02_output/tables", showWarnings = FALSE, recursive = TRUE)

# Generar tabla con stargazer (suprimir salida a consola)
invisible(capture.output(
  stargazer(model1, model2,
            type = "latex",
            title = "Age-Income Profile Regressions (Imputed Version)",
            label = "tab:age_income_imputed",
            dep.var.labels = "Log(Monthly Income)",
            covariate.labels = c("Age", "Age Squared", "Hours Worked"),
            omit = "factor\\(relab\\)",
            add.lines = list(
              c("Labor Relation FE", "No", "Yes"),
              c("Peak Age", sprintf("%.1f", peak_age_m1), sprintf("%.1f", peak_age_m2)),
              c("Sample", "Full (N=8,153)", "Full (N=8,153)")
            ),
            omit.stat = c("ser", "f"),
            digits = 4,
            out = "02_output/tables/age_income_regressions_imputed.tex")
))

cat("\nGuardado: 02_output/tables/age_income_regressions_imputed.tex\n")

# ==============================================================================
# PASO 6: BOOTSTRAP PARA INTERVALO DE CONFIANZA DE EDAD PICO
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 6: BOOTSTRAP - INTERVALO DE CONFIANZA EDAD PICO\n")
cat("================================================================================\n")

# Función para calcular edad pico en cada muestra bootstrap
peak_age_boot <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(log_income ~ age + age_squared, data = d)
  beta1 <- coef(model)["age"]
  beta2 <- coef(model)["age_squared"]
  peak <- -beta1 / (2 * beta2)
  return(peak)
}

cat("\nEjecutando bootstrap (500 repeticiones)...\n")
set.seed(12345)
boot_results <- boot(data = data, statistic = peak_age_boot, R = 500)

# Intervalo de confianza
ci_boot <- boot.ci(boot_results, type = "perc", conf = 0.95)

cat(sprintf("\nEdad de pico de ingresos: %.1f años\n", peak_age_m1))
cat(sprintf("IC 95%% (Bootstrap): [%.1f, %.1f]\n", 
            ci_boot$percent[4], ci_boot$percent[5]))

# Guardar resultados en tabla LaTeX manual
peak_table <- sprintf("\\begin{table}[h]
\\centering
\\caption{Peak Age of Earnings - Bootstrap Estimates (Imputed Version)}
\\label{tab:peak_age_imputed}
\\begin{tabular}{lc}
\\hline\\hline
& Estimate \\\\
\\hline
Peak Age (years) & %.2f \\\\
95\\%% CI Lower Bound & %.2f \\\\
95\\%% CI Upper Bound & %.2f \\\\
\\hline
Model & Unconditional \\\\
Bootstrap Replications & 500 \\\\
Sample & Full (N=8,153) \\\\
\\hline\\hline
\\end{tabular}
\\end{table}", peak_age_m1, ci_boot$percent[4], ci_boot$percent[5])

writeLines(peak_table, "02_output/tables/peak_age_estimates_imputed.tex")

cat("\nGuardado: 02_output/tables/peak_age_estimates_imputed.tex\n")

# ==============================================================================
# PASO 7: GRÁFICO DE PERFIL EDAD-INGRESO
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 7: GENERAR GRÁFICO DE PERFIL EDAD-INGRESO\n")
cat("================================================================================\n")

# Crear directorio si no existe
dir.create("02_output/figures", showWarnings = FALSE, recursive = TRUE)

# Crear grid de edades para predicción
age_grid <- data.frame(
  age = seq(18, 65, by = 1)
)
age_grid$age_squared <- age_grid$age^2
age_grid$totalHoursWorked <- median(data$totalHoursWorked)
age_grid$relab <- as.numeric(names(sort(table(data$relab), decreasing = TRUE))[1])

# Predicciones
age_grid$pred_m1 <- predict(model1, newdata = age_grid)
age_grid$pred_m2 <- predict(model2, newdata = age_grid)

# Convertir log-income a pesos
age_grid$income_m1 <- exp(age_grid$pred_m1)
age_grid$income_m2 <- exp(age_grid$pred_m2)

# Gráfico
p <- ggplot(age_grid, aes(x = age)) +
  geom_line(aes(y = income_m1, color = "Incondicional"), linewidth = 1.2) +
  geom_line(aes(y = income_m2, color = "Condicional"), linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = peak_age_m1, linetype = "dotted", color = "blue", linewidth = 0.8) +
  geom_vline(xintercept = peak_age_m2, linetype = "dotted", color = "red", linewidth = 0.8) +
  annotate("text", x = peak_age_m1 + 3, y = max(age_grid$income_m1) * 0.9, 
           label = sprintf("Pico: %.1f años", peak_age_m1), color = "blue", size = 4) +
  annotate("text", x = peak_age_m2 + 3, y = max(age_grid$income_m2) * 0.85, 
           label = sprintf("Pico: %.1f años", peak_age_m2), color = "red", size = 4) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Incondicional" = "blue", "Condicional" = "red"),
                     name = "Modelo") +
  labs(
    title = "Age-Income Profile (Imputed Version)",
    subtitle = "Predicted Monthly Labor Income by Age",
    x = "Age (years)",
    y = "Monthly Income (COP)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

ggsave("02_output/figures/age_income_profile_imputed.png", plot = p, 
       width = 10, height = 6, dpi = 300)

cat("\nGuardado: 02_output/figures/age_income_profile_imputed.png\n")

# ==============================================================================
# PASO 8: TABLA COMPARATIVA BASELINE VS IMPUTED
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 8: TABLA COMPARATIVA - BASELINE VS IMPUTED\n")
cat("================================================================================\n")

# Cargar dataset baseline
cat("\nCargando dataset baseline para comparación...\n")
data_baseline <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("  Baseline: N=%s\n", format(nrow(data_baseline), big.mark=",")))
cat(sprintf("  Imputed:  N=%s\n", format(nrow(data), big.mark=",")))

# Estimar mismos modelos en baseline
model1_baseline <- lm(log_income ~ age + age_squared, data = data_baseline)
model2_baseline <- lm(log_income ~ age + age_squared + totalHoursWorked + factor(relab), 
                      data = data_baseline)

# Calcular edades pico baseline
beta1_base <- coef(model1_baseline)["age"]
beta2_base <- coef(model1_baseline)["age_squared"]
peak_age_baseline <- -beta1_base / (2 * beta2_base)

beta1_m2_base <- coef(model2_baseline)["age"]
beta2_m2_base <- coef(model2_baseline)["age_squared"]
peak_age_m2_baseline <- -beta1_m2_base / (2 * beta2_m2_base)

cat("\nEdades de pico - Comparación:\n")
cat(sprintf("  Baseline (Incondicional): %.1f años\n", peak_age_baseline))
cat(sprintf("  Imputed (Incondicional):  %.1f años\n", peak_age_m1))
cat(sprintf("  Diferencia:               %.1f años\n\n", peak_age_m1 - peak_age_baseline))

cat(sprintf("  Baseline (Condicional):   %.1f años\n", peak_age_m2_baseline))
cat(sprintf("  Imputed (Condicional):    %.1f años\n", peak_age_m2))
cat(sprintf("  Diferencia:               %.1f años\n", peak_age_m2 - peak_age_m2_baseline))

# Generar tabla comparativa con stargazer
cat("\nGenerando tabla comparativa...\n")
invisible(capture.output(
  stargazer(model1_baseline, model1, model2_baseline, model2,
            type = "latex",
            title = "Age-Income Profile: Baseline vs Imputed Comparison",
            label = "tab:comparison",
            column.labels = c("Baseline", "Imputed", "Baseline", "Imputed"),
            dep.var.labels.include = FALSE,
            model.names = FALSE,
            column.separate = c(2, 2),
            covariate.labels = c("Age", "Age Squared", "Hours Worked"),
            omit = "factor\\(relab\\)",
            add.lines = list(
              c("Labor Relation FE", "No", "No", "Yes", "Yes"),
              c("Peak Age", 
                sprintf("%.1f", peak_age_baseline), 
                sprintf("%.1f", peak_age_m1),
                sprintf("%.1f", peak_age_m2_baseline),
                sprintf("%.1f", peak_age_m2)),
              c("Sample Size", 
                format(nrow(data_baseline), big.mark=","),
                format(nrow(data), big.mark=","),
                format(nrow(data_baseline), big.mark=","),
                format(nrow(data), big.mark=","))
            ),
            omit.stat = c("ser", "f"),
            digits = 4,
            out = "02_output/tables/comparison_baseline_imputed.tex")
))

cat("  Guardado: 02_output/tables/comparison_baseline_imputed.tex\n")

# ==============================================================================
# PASO 9: RESUMEN FINAL Y COMPARACIÓN
# ==============================================================================

cat("\n================================================================================\n")
cat("SECCIÓN 1 COMPLETADA (VERSIÓN IMPUTADA)\n")
cat("================================================================================\n")

cat("\nARCHIVOS GENERADOS:\n")
cat("\nTablas:\n")
cat("  1. 02_output/tables/age_income_regressions_imputed.tex\n")
cat("  2. 02_output/tables/peak_age_estimates_imputed.tex\n")
cat("  3. 02_output/tables/comparison_baseline_imputed.tex (COMPARACIÓN)\n")
cat("\nGráficos:\n")
cat("  1. 02_output/figures/age_income_profile_imputed.png\n")

cat("\nRESULTADOS CLAVE (VERSIÓN IMPUTADA):\n")
cat(sprintf("  Modelo 1 - R²: %.4f, Edad pico: %.1f años\n", 
            summary(model1)$r.squared, peak_age_m1))
cat(sprintf("  Modelo 2 - R²: %.4f, Edad pico: %.1f años\n", 
            summary(model2)$r.squared, peak_age_m2))
cat(sprintf("  IC 95%% Bootstrap: [%.1f, %.1f]\n", 
            ci_boot$percent[4], ci_boot$percent[5]))

cat("\nANÁLISIS DE ROBUSTEZ:\n")
cat(sprintf("  Muestra completa (N=8,153): Pico = %.1f años\n", peak_age_m1))
cat(sprintf("  Solo observados (N=7,201): Pico = %.1f años\n", peak_age_obs))
cat(sprintf("  Diferencia: %.1f años\n", peak_age_m1 - peak_age_obs))

cat("\nCOMPARACIÓN CON BASELINE:\n")
cat("  Baseline:  N=7,201 (drops missing income)\n")
cat(sprintf("  Imputed:   N=%s (imputes missing income)\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Diferencia: %s observaciones\n", format(nrow(data) - 7201, big.mark=",")))

cat("\n================================================================================\n")
