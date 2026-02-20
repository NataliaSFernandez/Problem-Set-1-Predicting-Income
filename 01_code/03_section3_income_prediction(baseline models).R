################################################################################
# PROBLEM SET 1: PREDICTING INCOME
# Section 3: Labor Income Prediction 
# Script 03: Income_Prediction.R
################################################################################
# OBJETIVO:
# Evaluar performance predictiva out-of-sample de modelos estimados en
# Secciones 1 y 2, utilizando enfoque de validation set.
#
# MODELOS EVALUADOS:
# - Sección 1: Age-income profile (incondicional y condicional)
# - Sección 2: Gender gap (incondicional y condicional)
#
# ENFOQUE:
# - Train: Chunks 1-7
# - Validation: Chunks 8-10
# - Métrica: Root Mean Squared Error (RMSE) en log_income
#
# INPUT:
#   - 00_data/cleaned/data_cleaned.csv (con columna chunk)
#
# OUTPUT:
#   - Tabla comparativa de RMSE para todos los modelos
#   - Archivos exportados en:
#       02_output/tables/03_section3_prediction/
#       02_output/figures/03_section3_prediction/
#
################################################################################

# Limpiar entorno
rm(list = ls())

# Instalar librerías
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stargazer)) install.packages("stargazer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(webshot)) install.packages("webshot")
if (!require(htmltools)) install.packages("htmltools")

# Cargar librerías
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(webshot)
  library(htmltools)
})

# Verificar phantomjs
if (!webshot::is_phantomjs_installed()) {
  cat("Instalando phantomjs para generación de PNG...\n")
  webshot::install_phantomjs()
}

cat("================================================================================\n")
cat("SECCIÓN 3: PREDICCIÓN DE INGRESO LABORAL\n")
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

# Verificar que existe columna chunk
if (!"chunk" %in% colnames(data)) {
  stop("ERROR: La columna 'chunk' no existe en los datos. Verificar limpieza.")
}

cat(sprintf("  Chunks disponibles: %s\n", paste(sort(unique(data$chunk)), collapse=", ")))

# ==============================================================================
# PASO 2: DIVIDIR DATOS EN ENTRENAMIENTO Y VALIDACIÓN
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 2: DIVIDIR DATOS EN TRAIN Y VALIDATION\n")
cat("================================================================================\n")

# Dividir datos según especificación del Problem Set
train_df <- subset(data, chunk %in% 1:7)
valid_df <- subset(data, chunk %in% 8:10)

cat(sprintf("\nTrain set:\n"))
cat(sprintf("  N: %s\n", format(nrow(train_df), big.mark=",")))
cat(sprintf("  Chunks: %s\n", paste(sort(unique(train_df$chunk)), collapse=", ")))

cat(sprintf("\nValidation set:\n"))
cat(sprintf("  N: %s\n", format(nrow(valid_df), big.mark=",")))
cat(sprintf("  Chunks: %s\n", paste(sort(unique(valid_df$chunk)), collapse=", ")))

# ==============================================================================
# PASO 3: ESTIMAR MODELOS DE SECCIÓN 1 (AGE-INCOME PROFILE)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 3: ESTIMAR MODELOS DE SECCIÓN 1 EN TRAIN SET\n")
cat("================================================================================\n")

# Modelo 1 (Sección 1): Incondicional - Age + Age²
cat("\nModelo 1 (Sección 1 - Incondicional): log_income ~ age + age²\n")
model1_train <- lm(log_income ~ age + age_squared, data = train_df)
cat(sprintf("  R² Train: %.4f\n", summary(model1_train)$r.squared))

# Modelo 2 (Sección 1): Condicional - Age + Age² + Hours + Labor Relation
cat("\nModelo 2 (Sección 1 - Condicional): log_income ~ age + age² + hours + relab\n")
model2_train <- lm(log_income ~ age + age_squared + totalHoursWorked + factor(relab), 
                   data = train_df)
cat(sprintf("  R² Train: %.4f\n", summary(model2_train)$r.squared))

# ==============================================================================
# PASO 4: ESTIMAR MODELOS DE SECCIÓN 2 (GENDER GAP)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 4: ESTIMAR MODELOS DE SECCIÓN 2 EN TRAIN SET\n")
cat("================================================================================\n")

# Modelo 3 (Sección 2): Gender gap incondicional
cat("\nModelo 3 (Sección 2 - Incondicional): log_income ~ female\n")
model3_train <- lm(log_income ~ female, data = train_df)
cat(sprintf("  R² Train: %.4f\n", summary(model3_train)$r.squared))

# Modelo 4 (Sección 2): Gender gap condicional
cat("\nModelo 4 (Sección 2 - Condicional): log_income ~ female + controles\n")

control_num <- c("age", "age_squared", "totalHoursWorked", "p6426", "p6240","formal")
control_fac <- c("maxEducLevel", "relab", "oficio", "sizeFirm", "estrato1")

# Convertir categóricas a factor en TRAIN
for(v in control_fac){
  train_df[[v]] <- factor(train_df[[v]])
}

# Alinear niveles en VALID con base en TRAIN
for(v in control_fac){
  valid_df[[v]] <- factor(valid_df[[v]], levels = levels(train_df[[v]]))
}

# Construir fórmula
rhs <- c(
  "female",
  control_num,
  paste0("factor(", control_fac, ")")
)
f4 <- as.formula(paste("log_income ~", paste(rhs, collapse = " + ")))

# Estimar modelo 4 en TRAIN
model4_train <- lm(f4, data = train_df)
cat(sprintf("  R² Train: %.4f\n", summary(model4_train)$r.squared))

# ==============================================================================
# PASO 5: PREDICCIÓN EN VALIDATION SET
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 5: PREDICCIÓN EN VALIDATION SET\n")
cat("================================================================================\n")

# Predicciones para cada modelo
valid_df$pred_model1 <- predict(model1_train, newdata = valid_df)
valid_df$pred_model2 <- predict(model2_train, newdata = valid_df)
valid_df$pred_model3 <- predict(model3_train, newdata = valid_df)
valid_df$pred_model4 <- predict(model4_train, newdata = valid_df)

# Verificar NAs en predicciones
for(i in 1:4){
  n_na <- sum(is.na(valid_df[[paste0("pred_model", i)]]))
  if(n_na > 0){
    cat(sprintf("AVISO: Modelo %d tiene %d predicciones NA\n", i, n_na))
  }
}

# ==============================================================================
# PASO 6: CALCULAR RMSE EN VALIDATION SET
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 6: CALCULAR RMSE EN VALIDATION SET\n")
cat("================================================================================\n")

# Función para calcular RMSE (ignora NAs)
calc_rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# Calcular RMSE para cada modelo
rmse_model1 <- calc_rmse(valid_df$log_income, valid_df$pred_model1)
rmse_model2 <- calc_rmse(valid_df$log_income, valid_df$pred_model2)
rmse_model3 <- calc_rmse(valid_df$log_income, valid_df$pred_model3)
rmse_model4 <- calc_rmse(valid_df$log_income, valid_df$pred_model4)

cat("\nRMSE en Validation Set:\n")
cat(sprintf("  Modelo 1 (Sección 1 - Incond.): %.4f\n", rmse_model1))
cat(sprintf("  Modelo 2 (Sección 1 - Cond.):   %.4f\n", rmse_model2))
cat(sprintf("  Modelo 3 (Sección 2 - Incond.): %.4f\n", rmse_model3))
cat(sprintf("  Modelo 4 (Sección 2 - Cond.):   %.4f\n", rmse_model4))

# Identificar mejor modelo
rmse_values <- c(rmse_model1, rmse_model2, rmse_model3, rmse_model4)
best_model_idx <- which.min(rmse_values)
cat(sprintf("\nMejor modelo (menor RMSE): Modelo %d con RMSE = %.4f\n", 
            best_model_idx, rmse_values[best_model_idx]))

# ==============================================================================
# PASO 7: TABLA COMPARATIVA DE MODELOS (TEX + PNG)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 7: GENERAR TABLA COMPARATIVA DE RMSE\n")
cat("================================================================================\n")

# Crear directorio si no existe
dir.create("02_output/tables/03_section3_prediction", showWarnings = FALSE, recursive = TRUE)

# Crear data frame con resultados
results_df <- data.frame(
  Modelo = c(
    "Sección 1 - Incondicional (age + age²)",
    "Sección 1 - Condicional (age + age² + hours + relab)",
    "Sección 2 - Incondicional (female)",
    "Sección 2 - Condicional (female + controles)"
  ),
  R2_Train = c(
    summary(model1_train)$r.squared,
    summary(model2_train)$r.squared,
    summary(model3_train)$r.squared,
    summary(model4_train)$r.squared
  ),
  RMSE_Validation = c(
    rmse_model1,
    rmse_model2,
    rmse_model3,
    rmse_model4
  )
)

# Marcar mejor modelo
results_df$Best <- ifelse(1:4 == best_model_idx, "*", "")

# Guardar como TEX usando stargazer
invisible(capture.output(
  stargazer(results_df,
            type = "latex",
            title = "Model Comparison: Out-of-Sample Predictive Performance",
            label = "tab:model_comparison",
            summary = FALSE,
            rownames = FALSE,
            digits = 4,
            out = "02_output/tables/03_section3_prediction/model_comparison.tex")
))

cat("\nGuardado: 02_output/tables/03_section3_prediction/model_comparison.tex\n")

# Generar versión HTML para PNG con fondo blanco
html_table <- sprintf("<!DOCTYPE html>
<html>
<head>
<meta charset='UTF-8'>
<style>
body { background-color: white; padding: 20px; font-family: Arial; }
h2 { text-align: center; }
table { background-color: white; border-collapse: collapse; margin: 20px auto; font-size: 14px; }
th, td { padding: 8px; border: 1px solid black; }
thead { background-color: #e0e0e0; }
p { text-align: center; font-size: 12px; }
</style>
</head>
<body>
<h2>Model Comparison: Out-of-Sample Predictive Performance</h2>
<table>
<thead>
<tr>
<th>Modelo</th>
<th>R%s (Train)</th>
<th>RMSE (Validation)</th>
<th>Best</th>
</tr>
</thead>
<tbody>
<tr><td>Secci%sn 1 - Incondicional</td><td>%.4f</td><td>%.4f</td><td>%s</td></tr>
<tr><td>Secci%sn 1 - Condicional</td><td>%.4f</td><td>%.4f</td><td>%s</td></tr>
<tr><td>Secci%sn 2 - Incondicional</td><td>%.4f</td><td>%.4f</td><td>%s</td></tr>
<tr><td>Secci%sn 2 - Condicional</td><td>%.4f</td><td>%.4f</td><td>%s</td></tr>
</tbody>
</table>
<p>
* Best model has lowest validation RMSE<br>
Train set: Chunks 1-7 (N=%s) | Validation set: Chunks 8-10 (N=%s)
</p>
</body>
</html>", 
"\u00B2", "\u00F3",
results_df$R2_Train[1], results_df$RMSE_Validation[1], results_df$Best[1],
"\u00F3",
results_df$R2_Train[2], results_df$RMSE_Validation[2], results_df$Best[2],
"\u00F3",
results_df$R2_Train[3], results_df$RMSE_Validation[3], results_df$Best[3],
"\u00F3",
results_df$R2_Train[4], results_df$RMSE_Validation[4], results_df$Best[4],
format(nrow(train_df), big.mark=","),
format(nrow(valid_df), big.mark=","))

html_temp <- tempfile(fileext = ".html")
writeLines(html_table, html_temp)

# Convertir a PNG
tryCatch({
  webshot(html_temp, 
          file = "02_output/tables/03_section3_prediction/model_comparison.png",
          vwidth = 900,
          vheight = 500)
  cat("Guardado: 02_output/tables/03_section3_prediction/model_comparison.png\n")
}, error = function(e) {
  cat("AVISO: No se pudo generar PNG (requiere phantomjs)\n")
})

unlink(html_temp)

# ==============================================================================
# PASO 8: GRÁFICO COMPARATIVO DE RMSE
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 8: GENERAR GRÁFICO COMPARATIVO\n")
cat("================================================================================\n")

# Crear directorio si no existe
dir.create("02_output/figures/03_section3_prediction", showWarnings = FALSE, recursive = TRUE)

# Preparar datos para gráfico
plot_data <- data.frame(
  Model = factor(1:4, labels = c("S1-Incond", "S1-Cond", "S2-Incond", "S2-Cond")),
  RMSE = rmse_values,
  Section = c("Section 1", "Section 1", "Section 2", "Section 2"),
  Best = ifelse(1:4 == best_model_idx, "Best", "Other")
)

# Crear gráfico
p <- ggplot(plot_data, aes(x = Model, y = RMSE, fill = Best)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.4f", RMSE)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Best" = "#00BA38", "Other" = "#619CFF")) +
  labs(
    title = "Model Comparison: Validation RMSE",
    subtitle = sprintf("Train: Chunks 1-7 (N=%s) | Validation: Chunks 8-10 (N=%s)",
                      format(nrow(train_df), big.mark=","),
                      format(nrow(valid_df), big.mark=",")),
    x = "Model",
    y = "RMSE (log income)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    legend.position = "top"
  )

ggsave("02_output/figures/03_section3_prediction/rmse_comparison.png", 
       plot = p, width = 10, height = 6, dpi = 300)

cat("\nGuardado: 02_output/figures/03_section3_prediction/rmse_comparison.png\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n================================================================================\n")
cat("SECCIÓN 3 COMPLETADA\n")
cat("================================================================================\n")

cat("\nARCHIVOS GENERADOS:\n")
cat("\nTablas:\n")
cat("  1. 02_output/tables/03_section3_prediction/model_comparison.tex\n")
cat("  2. 02_output/tables/03_section3_prediction/model_comparison.png\n")
cat("\nGráficos:\n")
cat("  1. 02_output/figures/03_section3_prediction/rmse_comparison.png\n")

cat("\nRESUMEN DE RESULTADOS:\n")
cat(sprintf("  Mejor modelo: Modelo %d\n", best_model_idx))
cat(sprintf("  RMSE mínimo: %.4f\n", min(rmse_values)))
cat("\nRanking de modelos (por RMSE):\n")
ranking <- order(rmse_values)
for(i in 1:4){
  idx <- ranking[i]
  cat(sprintf("  %d. Modelo %d: RMSE = %.4f\n", i, idx, rmse_values[idx]))
}
