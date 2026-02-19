################################################################################
# PROBLEM SET 1: PREDICTING INCOME
# Script 01: Age-Income Profile (Section 1)
################################################################################
# OBJETIVO: Estimar perfil edad-ingreso y calcular edad de pico de earnings
#
# INPUTS:  00_data/cleaned/data_cleaned.csv
# OUTPUTS: 
#   - 02_output/tables/01_section1_age_income/age_income_regressions.tex
#   - 02_output/tables/01_section1_age_income/age_income_regressions.png
#   - 02_output/figures/01_section1_age_income/age_income_profile.png
#   - 02_output/tables/01_section1_age_income/peak_age_estimates.tex
#   - 02_output/tables/01_section1_age_income/peak_age_estimates.png
################################################################################

# Limpiar entorno
rm(list = ls())

# Instalar librerías
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stargazer)) install.packages("stargazer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(boot)) install.packages("boot")
if (!require(webshot)) install.packages("webshot")
if (!require(htmltools)) install.packages("htmltools")

# Cargar librerías
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(boot)
  library(webshot)
  library(htmltools)
})

# Verificar si phantomjs está instalado (necesario para webshot)
if (!webshot::is_phantomjs_installed()) {
  cat("Instalando phantomjs para generación de PNG...\n")
  webshot::install_phantomjs()
}

cat("================================================================================\n")
cat("SECCIÓN 1: PERFIL EDAD-INGRESO\n")
cat("================================================================================\n")

# ==============================================================================
# PASO 1: CARGAR DATOS
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 1: CARGAR DATOS LIMPIOS\n")
cat("================================================================================\n")

data <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data)))

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
# PASO 4: TABLA DE REGRESIONES (TEX + PNG)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 4: GENERAR TABLA DE REGRESIONES (TEX + PNG)\n")
cat("================================================================================\n")

# Crear directorio si no existe
dir.create("02_output/tables/01_section1_age_income", showWarnings = FALSE, recursive = TRUE)

# Generar tabla TEX
invisible(capture.output(
  stargazer(model1, model2,
            type = "latex",
            title = "Age-Income Profile Regressions",
            label = "tab:age_income",
            dep.var.labels = "Log(Monthly Income)",
            covariate.labels = c("Age", "Age Squared", "Hours Worked"),
            omit = "factor\\(relab\\)",
            add.lines = list(
              c("Labor Relation FE", "No", "Yes"),
              c("Peak Age", sprintf("%.1f", peak_age_m1), sprintf("%.1f", peak_age_m2))
            ),
            omit.stat = c("ser", "f"),
            digits = 4,
            out = "02_output/tables/01_section1_age_income/age_income_regressions.tex")
))

cat("\nGuardado: 02_output/tables/01_section1_age_income/age_income_regressions.tex\n")

# Generar tabla HTML (para convertir a PNG)
html_temp <- tempfile(fileext = ".html")

# Agregar CSS para fondo blanco
html_wrapper <- sprintf('
<!DOCTYPE html>
<html>
<head>
<style>
body { background-color: white; padding: 20px; }
table { background-color: white; }
</style>
</head>
<body>
%s
</body>
</html>
', paste(readLines(textConnection(capture.output(
  stargazer(model1, model2,
            type = "html",
            title = "Age-Income Profile Regressions",
            dep.var.labels = "Log(Monthly Income)",
            covariate.labels = c("Age", "Age Squared", "Hours Worked"),
            omit = "factor\\(relab\\)",
            add.lines = list(
              c("Labor Relation FE", "No", "Yes"),
              c("Peak Age", sprintf("%.1f", peak_age_m1), sprintf("%.1f", peak_age_m2))
            ),
            omit.stat = c("ser", "f"),
            digits = 4)
))), collapse = "\n"))

writeLines(html_wrapper, html_temp)

# Convertir HTML a PNG
tryCatch({
  webshot(html_temp, 
          file = "02_output/tables/01_section1_age_income/age_income_regressions.png",
          vwidth = 800,
          vheight = 600)
  cat("Guardado: 02_output/tables/01_section1_age_income/age_income_regressions.png\n")
}, error = function(e) {
  cat("AVISO: No se pudo generar PNG (requiere phantomjs)\n")
  cat("Ejecute: webshot::install_phantomjs()\n")
})

# Limpiar archivo temporal
unlink(html_temp)

# ==============================================================================
# PASO 5: BOOTSTRAP PARA INTERVALO DE CONFIANZA DE EDAD PICO
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 5: BOOTSTRAP - INTERVALO DE CONFIANZA EDAD PICO\n")
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

cat("\nEjecutando bootstrap (1000 repeticiones)...\n")
set.seed(12345)
boot_results <- boot(data = data, statistic = peak_age_boot, R = 1000)

# Intervalo de confianza
ci_boot <- boot.ci(boot_results, type = "perc", conf = 0.95)

cat(sprintf("\nEdad de pico de ingresos: %.1f años\n", peak_age_m1))
cat(sprintf("IC 95%% (Bootstrap): [%.1f, %.1f]\n", 
            ci_boot$percent[4], ci_boot$percent[5]))

# Guardar resultados en tabla TEX
peak_table <- sprintf("\\begin{table}[h]
\\centering
\\caption{Peak Age of Earnings - Bootstrap Estimates}
\\label{tab:peak_age}
\\begin{tabular}{lc}
\\hline\\hline
& Estimate \\\\
\\hline
Peak Age (years) & %.2f \\\\
95\\%% CI Lower Bound & %.2f \\\\
95\\%% CI Upper Bound & %.2f \\\\
\\hline
Model & Unconditional \\\\
Bootstrap Replications & 1000 \\\\
\\hline\\hline
\\end{tabular}
\\end{table}", peak_age_m1, ci_boot$percent[4], ci_boot$percent[5])

writeLines(peak_table, "02_output/tables/01_section1_age_income/peak_age_estimates.tex")
cat("\nGuardado: 02_output/tables/01_section1_age_income/peak_age_estimates.tex\n")

# Generar versión HTML para PNG con fondo blanco
peak_html <- sprintf("<!DOCTYPE html>
<html>
<head>
<style>
body { background-color: white; padding: 20px; font-family: Arial; }
table { background-color: white; border-collapse: collapse; margin: 20px; }
td, th { padding: 8px; border: 1px solid black; }
th { background-color: #f0f0f0; }
caption { font-size: 16px; font-weight: bold; margin-bottom: 10px; }
</style>
</head>
<body>
<table>
<caption>Peak Age of Earnings - Bootstrap Estimates</caption>
<tr><th></th><th>Estimate</th></tr>
<tr><td>Peak Age (years)</td><td>%.2f</td></tr>
<tr><td>95%% CI Lower Bound</td><td>%.2f</td></tr>
<tr><td>95%% CI Upper Bound</td><td>%.2f</td></tr>
<tr style='background-color:#f0f0f0;'><td colspan='2' style='text-align:center;'><b>Model Details</b></td></tr>
<tr><td>Model</td><td>Unconditional</td></tr>
<tr><td>Bootstrap Replications</td><td>1000</td></tr>
</table>
</body>
</html>", peak_age_m1, ci_boot$percent[4], ci_boot$percent[5])

html_temp2 <- tempfile(fileext = ".html")
writeLines(peak_html, html_temp2)

# Convertir a PNG
tryCatch({
  webshot(html_temp2, 
          file = "02_output/tables/01_section1_age_income/peak_age_estimates.png",
          vwidth = 600,
          vheight = 400)
  cat("Guardado: 02_output/tables/01_section1_age_income/peak_age_estimates.png\n")
}, error = function(e) {
  cat("AVISO: No se pudo generar PNG (requiere phantomjs)\n")
})

unlink(html_temp2)

# ==============================================================================
# PASO 6: GRÁFICO DE PERFIL EDAD-INGRESO
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 6: GENERAR GRÁFICO DE PERFIL EDAD-INGRESO\n")
cat("================================================================================\n")

# Crear directorio si no existe
dir.create("02_output/figures/01_section1_age_income", showWarnings = FALSE, recursive = TRUE)

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
    title = "Age-Income Profile",
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

ggsave("02_output/figures/01_section1_age_income/age_income_profile.png", plot = p, 
       width = 10, height = 6, dpi = 300)

cat("\nGuardado: 02_output/figures/01_section1_age_income/age_income_profile.png\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n================================================================================\n")
cat("SECCIÓN 1 COMPLETADA\n")
cat("================================================================================\n")

cat("\nARCHIVOS GENERADOS:\n")
cat("\nTablas:\n")
cat("  1. 02_output/tables/01_section1_age_income/age_income_regressions.tex\n")
cat("  2. 02_output/tables/01_section1_age_income/age_income_regressions.png\n")
cat("  3. 02_output/tables/01_section1_age_income/peak_age_estimates.tex\n")
cat("  4. 02_output/tables/01_section1_age_income/peak_age_estimates.png\n")
cat("\nGráficos:\n")
cat("  1. 02_output/figures/01_section1_age_income/age_income_profile.png\n")

cat("\nRESULTADOS CLAVE:\n")
cat(sprintf("  Modelo 1 - R²: %.4f, Edad pico: %.1f años\n", 
            summary(model1)$r.squared, peak_age_m1))
cat(sprintf("  Modelo 2 - R²: %.4f, Edad pico: %.1f años\n", 
            summary(model2)$r.squared, peak_age_m2))
cat(sprintf("  IC 95%% Bootstrap: [%.1f, %.1f]\n", 
            ci_boot$percent[4], ci_boot$percent[5]))

cat("\n================================================================================\n")
