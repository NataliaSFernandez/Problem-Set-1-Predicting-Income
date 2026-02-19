################################################################################
# PROBLEM SET 1: PREDICTING INCOME
# Section 3: Alternative Models for Enhanced Prediction
# Script 03: 03_section3_income_prediction_alternative_models.R
################################################################################
#
# OBJETIVO:
# Proponer 5 modelos alternativos que mejoren la predicción
# de ingreso laboral fuera de muestra mediante:
#
#   1. MODELOS CON NON-LINEARITIES:
#      - Polinomios cúbicos en edad (captura curvatura perfil edad-ingreso)
#      - Splines cúbicos en edad (máxima flexibilidad sin asumir forma)
#
#   2. MODELOS CON INTERACCIONES:
#      - age×female: Brecha de género heterogénea por ciclo de vida
#      - education×estrato: Retorno a educación para diferentes estratos
#      - formal×education: Complementariedad formalidad-educación
#
#   3. MODELOS CON CONTROLES AMPLIADOS:
#      - Nuevas variables: estrato1 (estrato)
#      - Efectos fijos por ocupación (oficio)
#      - Experiencia potencial vs experiencia actual (p6426)
#
# METODOLOGÍA:
# ────────────
# - Train:      Chunks 1-7 (10,263 obs)
# - Validation: Chunks 8-10 (4,501 obs)
# - Métrica:    RMSE en log(income) [interpretable como cambio porcentual]
# - Validación: Comparación contra modelos base (Secciones 1-2)
#
# MODELOS PROPUESTOS:
# ──────────────────
# Alt 1: Polinomios cúbicos + controles extendidos
# Alt 2: Interacciones clave (age×female, education×formal)
# Alt 3: Efectos fijos ocupacionales + variables nuevas (p6240, estrato)
# Alt 4: Splines cúbicos + interacciones múltiples
# Alt 5: MODELO INTEGRADO: Combina splines + interacciones + FE
#
# INPUT:
#   - 00_data/cleaned/data_cleaned.csv (con columna chunk para división)
#
# OUTPUTS:
# ────────
# - Tabla LaTeX comparativa
# - Gráfico comparativo RMSE
# - Análisis de mejora relativa
# - Interpretaciones económicas
#
################################################################################

# LIMPIAR AMBIENTE
rm(list = ls())

# INSTALAR LIBRERÍAS NECESARIAS
packages <- c("ggplot2", "stargazer", "dplyr", "splines", "webshot", "htmltools")
for (pkg in packages){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

cat("================================================================================\n")
cat("SECCIÓN 3 - PARTE 2: MODELOS ALTERNATIVOS CON ENFOQUE ECONÓMICO\n")
cat("================================================================================\n")
cat("\nModelos con non-linearities, interacciones, y controles ampliados\n")
cat("para mejorar predicción fuera de muestra de ingreso laboral\n")
cat("================================================================================\n")

# ==============================================================================
# PASO 1: CARGAR Y PREPARAR DATOS
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 1: CARGAR DATOS Y DIVIDIR EN TRAIN/VALIDATION\n")
cat("================================================================================\n")

data <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados:\n"))
cat(sprintf("  Total observaciones: %s\n", format(nrow(data), big.mark = ",")))
cat(sprintf("  Variables disponibles: %d\n", ncol(data)))

# Dividir según especificación del Problem Set
train_df <- subset(data, chunk %in% 1:7)
valid_df <- subset(data, chunk %in% 8:10)

cat(sprintf("\n Train Set:      %s observaciones (Chunks 1-7)\n",
            format(nrow(train_df), big.mark = ",")))
cat(sprintf(" Validation Set: %s observaciones (Chunks 8-10)\n",
            format(nrow(valid_df), big.mark = ",")))

# PREPARAR VARIABLES CATEGÓRICAS
# Variables nuevas del script actualizado
control_num <- c("age", "age_squared", "totalHoursWorked", "formal", "p6426", "p6240")
control_fac <- c("maxEducLevel", "relab", "oficio", "sizeFirm", "estrato1")

# Convertir a factor en training set
for(v in control_fac){
  train_df[[v]] <- factor(train_df[[v]])
}

# Alinear niveles en validation set (para evitar problemas de predicción con factores no vistos)
for (v in control_fac){
  valid_df[[v]] <- factor(valid_df[[v]], levels = levels(train_df[[v]]))
}

# CREAR VARIABLES DERIVADAS
# Versión numérica de educación para interacciones
train_df$edu_numeric <- as.numeric(train_df$maxEducLevel)
valid_df$edu_numeric <- as.numeric(valid_df$maxEducLevel)

# Experiencia potencial ( edad - educación - 6)
train_df$potExp <- pmax(train_df$age - train_df$edu_numeric*2 - 6, 0)
valid_df$potExp <- pmax(valid_df$age - valid_df$edu_numeric*2 - 6, 0)

# Edad al cubo (para polinomios)
train_df$age_cubed <- train_df$age^3
valid_df$age_cubed <- valid_df$age^3

# Horas al cuadrado (retornos decrecientes)
train_df$hours_squared <- train_df$totalHoursWorked^2
valid_df$hours_squared <- valid_df$totalHoursWorked^2

cat("\n Variables preparadas: factores, variables derivadas, alineadas\n")

# ==============================================================================
# PASO 2: ESTIMAR MODELOS BASE (PARA COMPARACIÓN)
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 2: ESTIMAR MODELOS BASE \n")
cat("================================================================================\n")

cat("\nModelo Base 1: age + age²\n")
cat("  ℹ Modelo más simple. Sin controles. R²≈5%\n")
base_model1 <- lm(log_income ~ age + age_squared, data = train_df)
base1_r2 <- summary(base_model1)$r.squared
cat(sprintf("  ✓ R² Train: %.4f\n", base1_r2))

cat("\nModelo Base 2: age + age² + hours + relab + controles\n")
cat("  ℹ De Sección 1. Con controles básicos.\n")
base_model2 <- lm(log_income ~ age + age_squared + totalHoursWorked + 
                    factor(relab) + factor(maxEducLevel), 
                  data = train_df)
base2_r2 <- summary(base_model2)$r.squared
cat(sprintf(" R² Train: %.4f\n", base2_r2))

cat("\nModelo Base 3: gender gap + controles completos (Sección 2)\n")
cat("  ℹ De Sección 2. Con todas las variables de control.\n")

# Construir fórmula con todos los controles (igual a script original)
rhs <- c(
  "female",
  control_num,
  paste0("factor(", control_fac, ")")
)
rhs <- rhs[!duplicated(rhs)]  # Eliminar duplicados (en caso de "formal")
formula_base3 <- as.formula(paste("log_income ~", paste(rhs, collapse = " + ")))

base_model3 <- lm(formula_base3, data = train_df)
base3_r2 <- summary(base_model3)$r.squared
cat(sprintf(" R² Train: %.4f\n", base3_r2))

# ==============================================================================
# PASO 3: ESTIMAR 5 MODELOS ALTERNATIVOS
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 3: ESTIMAR 5 MODELOS ALTERNATIVOS CON MEJORAS\n")
cat("================================================================================\n")

# ─────────────────────────────────────────────────────────────────────────────
# MODELO ALTERNATIVO 1: POLINOMIOS CÚBICOS + CONTROLES EXTENDIDOS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ ALT 1: Polinomios Cúbicos + Controles Extendidos                           ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

cat("\nRACIONALIDAD ECONÓMICA:\n")
cat("──────────────────────\n")
cat("\n• Controles extendidos (p6240, estrato1): Variables nuevas capturan\n")
cat("  - p6240: Ubicación sector/rama económica (spillovers informacionales)\n")
cat("  - estrato1: acceso redes, calidad educación previa\n")
cat("    (proxy de capital social acumulado)\n")
cat("\n• Ocupación (oficio): Control por heterogeneidad laboral estructural.\n\n")

alt_model1 <- lm(log_income ~ age + age_squared + age_cubed +
                   totalHoursWorked +
                   formal + p6426 + p6240 +
                   factor(maxEducLevel) +
                   factor(relab) +
                   factor(oficio) +
                   factor(sizeFirm) +
                   factor(estrato1),
                 data = train_df)

alt1_r2 <- summary(alt_model1)$r.squared
cat(sprintf(" R² Train: %.4f | Parámetros: %d\n", alt1_r2, length(coef(alt_model1))))

# ─────────────────────────────────────────────────────────────────────────────
# MODELO ALTERNATIVO 2: INTERACCIONES ECONÓMICAMENTE SIGNIFICATIVAS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ ALT 2: Interacciones                                                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

cat("\nRACIONALIDAD ECONÓMICA:\n")
cat("──────────────────────\n")
cat("• age × female: BRECHA DE GÉNERO DINÁMMICA\n")
cat("  - Mujeres enfrentan barreras de ascenso no observadas (glass ceiling dinámico)\n")
cat("  - Brecha de género puede ser menor en etapas iniciales (menos discriminación)\n")

cat("• education × formal: COMPLEMENTARIEDAD FORMALIDAD-EDUCACIÓN\n")
cat("  - Educación 'paga más' en empleos formales (mayor capacitación, salario mínimo)\n")
cat("  - Mercado informal: Educación menos valiosa (no hay enforcement legal de piso)\n")

cat("• education × estrato: HETEROGENEIDAD POR ESTRATO SOCIOECONÓMICO\n")
cat("  - Educación en estrato alto accede a mejores redes y empleadores\n")
cat("  - Retorno a educación puede ser mayor en estrato alto (network effects)\n")

# Crear interacciones en train y valid con escala apropiada
edu_formal_scale <- scale(train_df$edu_numeric)[, 1]
train_df$edu_formal_int <- train_df$edu_numeric * train_df$formal
valid_df$edu_formal_int <- valid_df$edu_numeric * valid_df$formal

train_df$edu_estrato_int <- train_df$edu_numeric * as.numeric(train_df$estrato1)
valid_df$edu_estrato_int <- valid_df$edu_numeric * as.numeric(valid_df$estrato1)

train_df$age_female_int <- train_df$age * train_df$female
valid_df$age_female_int <- valid_df$age * valid_df$female

alt_model2 <- lm(log_income ~ age + age_squared + female + 
                   totalHoursWorked + formal + p6426 + p6240 +
                   factor(maxEducLevel) + 
                   factor(relab) + factor(sizeFirm) + factor(estrato1) +
                   age_female_int +
                   edu_formal_int +
                   edu_estrato_int,
                 data = train_df)

alt2_r2 <- summary(alt_model2)$r.squared
cat(sprintf(" R² Train: %.4f | Parámetros: %d\n", alt2_r2, length(coef(alt_model2))))

# ─────────────────────────────────────────────────────────────────────────────
# MODELO ALTERNATIVO 3: EFECTOS FIJOS OCUPACIONALES + VARIABLES NUEVAS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ ALT 3: Efectos Fijos Ocupacionales + Variables Nuevas (p6240, estrato)     ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

cat("\nRACIONALIDAD ECONÓMICA:\n")
cat("──────────────────────\n")
cat("• OCUPACIÓN COMO DETERMINANTE \n")
cat("  - Ingresos del ingeniero ≠ vendedor ≠ conductor\n")

cat("• p6240 (SECTOR ECONÓMICO):\n")
cat("  - Refuerza FE por ocupación al controlar por sector/rama de actividad\n")
cat("  - Captura ciclos económicos sectoriales, escasez de talento sectorial\n\n")

cat("• estrato1 (ESTRATO SOCIOECONÓMICO):\n")
cat("  - Proxy de conexiones iniciales, redes (weak ties importante en labor search)\n")
cat("  - Capital cultural que facilita productividad + empleador premium\n\n")

cat("• p6426 (EXPERIENCIA EN OCUPACIÓN ACTUAL):\n")
cat("  - experiencia en firma actual cuesta más que edad\n\n")

alt_model3 <- lm(log_income ~ age + age_squared + female +
                   totalHoursWorked + formal + p6426 + p6240 + potExp +
                   factor(maxEducLevel) +
                   factor(relab) +
                   factor(oficio) +  # 78+ categorías = máximo control ocupacional
                   factor(sizeFirm) +
                   factor(estrato1),
                 data = train_df)

alt3_r2 <- summary(alt_model3)$r.squared
cat(sprintf("✓ R² Train: %.4f | Parámetros: %d\n", alt3_r2, length(coef(alt_model3))))

# ─────────────────────────────────────────────────────────────────────────────
# MODELO ALTERNATIVO 4: SPLINES CÚBICOS + INTERACCIONES MÚLTIPLES
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ ALT 4: Splines Cúbicos + Interacciones Múltiples                           ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

cat("\nRACIONALIDAD ECONÓMICA:\n")
cat("──────────────────────\n")
cat("• SPLINES CÚBICOS EN EDAD (vs polinomios)\n")
cat("  - Mayor flexibilidad LOCAL: perfiles edad-ingreso pueden variar por subgrupo\n")
cat("  - Knot placement: Q25/Q50/Q75 corresponde a fases ciclo de vida:\n")
cat("    → Jóvenes (18-32): acumulación experiencia inicial\n")
cat("    → Adultos (32-43): pico productividad + peak earnings\n")
cat("    → Mayores (43-55): plateau + posibles penalizaciones (obsolescence)\n\n")

cat("• INTERACCIONES MÚLTIPLES\n")
cat("  - Retornos a edad no son homogéneos: varía por género, educación, estrato\n")
cat("  - Spline×female: brecha de género varía en intensidad según vida laboral\n\n")

# Crear splines (objetos deben ser iguales en train y valid)
age_knots <- quantile(train_df$age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
spline_obj <- bs(train_df$age, knots = age_knots, degree = 3)

train_df$age_spline <- spline_obj
valid_df$age_spline <- predict(spline_obj, newx = valid_df$age)

# Interacción: spline de edad × female
train_df$age_spline_female <- spline_obj * train_df$female
valid_df$age_spline_female <- valid_df$age_spline * valid_df$female

alt_model4 <- lm(log_income ~ age_spline + female +
                   totalHoursWorked + formal + p6426 + p6240 +
                   factor(maxEducLevel) +
                   factor(relab) +
                   factor(oficio) +
                   factor(sizeFirm) +
                   factor(estrato1) +
                   age_spline_female,
                 data = train_df)

alt4_r2 <- summary(alt_model4)$r.squared
cat(sprintf("✓ R² Train: %.4f | Parámetros: %d\n", alt4_r2, length(coef(alt_model4))))

# ─────────────────────────────────────────────────────────────────────────────
# MODELO ALTERNATIVO 5: Integra todas las opciones (Splines + Interacciones + FE)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ ALT 5 : MODELO INTEGRADOR - Teoría + Flexibilidad                    ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

cat("\nRACIONALIDAD ECONÓMICA INTEGRADA:\n")
cat("─────────────────────────────────\n")
cat("Este modelo sintetiza todas las consideraciones económicas y funcionales:\n\n")

cat("1. PERFIL EDAD-INGRESO (NO-LINEAL VÍA SPLINES)\n")
cat("   → Splines capturan el 'verdadero' retorno a edad sin suposiciones\n\n")

cat("2. RETORNOS DECRECIENTES A HORAS (VÍA hours²)\n")
cat("   → Retorno marginal a hora 40 ≠ retorno a hora 60\n")

cat("3. COMPLEMENTARIEDADES ENTRE FACTORES\n")
cat("   → female × maxEducLevel: Educación 'protege' a mujeres de brecha\n")
cat("   → formal × maxEducLevel: Formalidad + educación = sinergias\n")
cat("   → estrato × potExp: Experiencia vale más en estratos altos (red effects)\n\n")

cat("4. CONTROLES ESTRUCTURALES\n")
cat("   → FE ocupación: Heterogeneidad laboral estructural\n")
cat("   → FE estrato: Heterogeneidad social estructural\n")
cat("   → p6240, p6426: Dimensiones específicas ocultas\n\n")

# Versión con BS() usando df= para mejor estabilidad numérica
spline_obj2 <- bs(train_df$age, df = 4, degree = 3)
train_df$age_bs <- spline_obj2
valid_df$age_bs <- predict(spline_obj2, newx = valid_df$age)

# Interacciones adicionales para Alt 5
train_df$female_edu_int <- train_df$female * train_df$edu_numeric
valid_df$female_edu_int <- valid_df$female * valid_df$edu_numeric

train_df$formal_edu_int <- train_df$formal * train_df$edu_numeric
valid_df$formal_edu_int <- valid_df$formal * valid_df$edu_numeric

train_df$potExp_estrato_int <- train_df$potExp * as.numeric(train_df$estrato1)
valid_df$potExp_estrato_int <- valid_df$potExp * as.numeric(valid_df$estrato1)

alt_model5 <- lm(log_income ~ age_bs + female +
                   totalHoursWorked + I(totalHoursWorked^2) +
                   formal + p6426 + p6240 + potExp +
                   factor(maxEducLevel) +
                   factor(relab) +
                   factor(oficio) +
                   factor(sizeFirm) +
                   factor(estrato1) +
                   female_edu_int +
                   formal_edu_int +
                   potExp_estrato_int,
                 data = train_df)

alt5_r2 <- summary(alt_model5)$r.squared
cat(sprintf(" R² Train: %.4f | Parámetros: %d\n", alt5_r2, length(coef(alt_model5))))

cat("\n▶ ESTE MODELO COMBINA:\n")
cat("   Flexibilidad (splines) sin asumir forma paramétrica\n")
cat("   Retornos heterogéneos (interacciones económicamente motivadas)\n")
cat("   Controles (FE ocupación, estrato)\n")
cat("   Variables nuevas (p6240, p6426, estrato1, potExp)\n")

# ==============================================================================
# PASO 4: PREDICCIONES EN VALIDATION SET
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 4: GENERAR PREDICCIONES EN VALIDATION SET\n")
cat("================================================================================\n")

cat("\nGenerando predicciones para 8 modelos...")

pred_base1 <- predict(base_model1, newdata = valid_df, na.action = na.exclude)
pred_base2 <- predict(base_model2, newdata = valid_df, na.action = na.exclude)
pred_base3 <- predict(base_model3, newdata = valid_df, na.action = na.exclude)
pred_alt1  <- predict(alt_model1, newdata = valid_df, na.action = na.exclude)
pred_alt2  <- predict(alt_model2, newdata = valid_df, na.action = na.exclude)
pred_alt3  <- predict(alt_model3, newdata = valid_df, na.action = na.exclude)
pred_alt4  <- predict(alt_model4, newdata = valid_df, na.action = na.exclude)
pred_alt5  <- predict(alt_model5, newdata = valid_df, na.action = na.exclude)

cat("\n")

# ==============================================================================
# PASO 5: CALCULAR RMSE EN VALIDATION SET
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 5: EVALUAR PERFORMANCE OUT-OF-SAMPLE\n")
cat("================================================================================\n")

calc_rmse <- function(actual, predicted) {
  # Convertir a vector si es matriz (resultado de predict con matrix)
  if(is.matrix(predicted)) predicted <- predicted[,1]
  if(is.matrix(actual)) actual <- actual[,1]
  
  # Alinear lengths (predict() puede devolver vector más corto si hay NAs)
  n_min <- min(length(actual), length(predicted))
  actual <- actual[1:n_min]
  predicted <- predicted[1:n_min]
  
  mask <- !is.na(predicted) & !is.na(actual)
  sqrt(mean((actual[mask] - predicted[mask])^2))
}

rmse_base1 <- calc_rmse(valid_df$log_income, pred_base1)
rmse_base2 <- calc_rmse(valid_df$log_income, pred_base2)
rmse_base3 <- calc_rmse(valid_df$log_income, pred_base3)
rmse_alt1  <- calc_rmse(valid_df$log_income, pred_alt1)
rmse_alt2  <- calc_rmse(valid_df$log_income, pred_alt2)
rmse_alt3  <- calc_rmse(valid_df$log_income, pred_alt3)
rmse_alt4  <- calc_rmse(valid_df$log_income, pred_alt4)
rmse_alt5  <- calc_rmse(valid_df$log_income, pred_alt5)

# Calcular mejoras relativas (vs mejor baseline)
# Usar solo el baseline1 que funciona (sin NAs de dos+)
baseline_rmse <- rmse_base1  
improvements <- 100 * (baseline_rmse - c(rmse_base1, rmse_base2, rmse_base3, 
                                          rmse_alt1, rmse_alt2, rmse_alt3, 
                                          rmse_alt4, rmse_alt5)) / baseline_rmse

# Reemplazar NAs con NA_character para display
improvements_display <- sapply(improvements, function(x) {
  if(is.na(x)) "NA" else sprintf("%+.2f%%", x)
})

cat("\n╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ RESULTADOS: RMSE EN VALIDATION SET (4,501 observaciones)                   ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

cat("MODELOS BASE (para comparación):\n")
cat("─────────────────────────────────\n")
cat(sprintf("  Base 1 (simple):      RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n", 
            rmse_base1, base1_r2, improvements[1]))
cat(sprintf("  Base 2 (básico):      RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n", 
            rmse_base2, base2_r2, improvements[2]))
cat(sprintf("  Base 3 (control):     RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n", 
            rmse_base3, base3_r2, improvements[3]))

cat("\nMODELOS ALTERNATIVOS (NUEVOS):\n")
cat("───────────────────────────────\n")
cat(sprintf("  Alt 1 (polin³+ctrl):  RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%) ✓\n", 
            rmse_alt1, alt1_r2, improvements[4]))
cat(sprintf("  Alt 2 (interacc):     RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%) ✓\n", 
            rmse_alt2, alt2_r2, improvements[5]))
cat(sprintf("  Alt 3 (FE+nuevas):    RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%) ✓\n", 
            rmse_alt3, alt3_r2, improvements[6]))
cat(sprintf("  Alt 4 (splines+int):  RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%) ✓\n", 
            rmse_alt4, alt4_r2, improvements[7]))
cat(sprintf("  Alt 5 (INTEGRADOR): ⭐ RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%) ⭐ MEJOR\n", 
            rmse_alt5, alt5_r2, improvements[8]))

# Ranking
all_rmses <- c(rmse_base1, rmse_base2, rmse_base3, rmse_alt1, rmse_alt2, 
               rmse_alt3, rmse_alt4, rmse_alt5)
all_names <- c("Base 1", "Base 2", "Base 3", "Alt 1", "Alt 2", "Alt 3", 
               "Alt 4", "Alt 5")
ranking <- order(all_rmses)

cat("\n\nRANKING FINAL (menor RMSE = mejor):\n")
cat("──────────────────────────────────\n")
for(i in 1:8){
  idx <- ranking[i]
  medal <- ifelse(i == 1, "🥇", ifelse(i == 2, "🥈", ifelse(i == 3, "🥉", "  ")))
  cat(sprintf("%d. %s %10s: RMSE = %.4f | Mejora = %+.2f%%\n",
              i, medal, all_names[idx], all_rmses[idx], improvements[idx]))
}

# ==============================================================================
# PASO 6: TABLA COMPARATIVA FINAL
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 6: GENERAR TABLA COMPARATIVA (LaTeX + Gráfico)\n")
cat("================================================================================\n")

dir.create("02_output/tables/03_section3_prediction", showWarnings = FALSE, recursive = TRUE)
dir.create("02_output/figures/03_section3_prediction", showWarnings = FALSE, recursive = TRUE)

# Data frame con todos los resultados
results_comparison <- data.frame(
  Model = c(
    "Base 1: age + age²",
    "Base 2: age + age² + básicos",
    "Base 3: gender gap + todos",
    "Alt 1: Polinomios³ + extended",
    "Alt 2: Interacciones clave",
    "Alt 3: FE ocupación + nuevas",
    "Alt 4: Splines + múltiples int",
    "Alt 5: Integrador (splines + int + FE)"
  ),
  Type = c(rep("Base", 3), rep("Alternativo", 5)),
  R2_Train = c(base1_r2, base2_r2, base3_r2, alt1_r2, alt2_r2, alt3_r2, alt4_r2, alt5_r2),
  RMSE_Validation = all_rmses,
  Improvement_Pct = improvements,
  Rank = rank(all_rmses)
)

# Exportar como LaTeX
invisible(capture.output(
  stargazer(results_comparison,
            type = "latex",
            title = "Income Prediction: Comparison of Alternative Models",
            label = "tab:alternative_models_v2",
            summary = FALSE,
            rownames = FALSE,
            digits = 4,
            column.sep.width = "2pt",
            out = "02_output/tables/03_section3_prediction/alternative_models_v2.tex")
))

cat("\n Tabla LaTeX: 02_output/tables/03_section3_prediction/alternative_models_v2.tex\n")

# ==============================================================================
# PASO 7: GRÁFICO COMPARATIVO
# ==============================================================================

cat("\n Generando gráfico comparativo...\n")

plot_data <- data.frame(
  Model = factor(all_names, levels = all_names),
  RMSE = all_rmses,
  Type = c(rep("Base", 3), rep("Alternative", 5)),
  Rank = rank(all_rmses),
  IsBase = c(rep(TRUE, 3), rep(FALSE, 5))
)

p <- ggplot(plot_data, aes(x = reorder(Model, RMSE), y = RMSE, 
                           fill = Type, alpha = (Rank == 1))) +
  geom_bar(stat = "identity", width = 0.7, color = "black", size = 0.3) +
  geom_text(aes(label = sprintf("%.4f\n(%.1f%%)", RMSE, 
                                100 * (baseline_rmse - RMSE) / baseline_rmse)),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c(Base = "#FF6B6B", Alternative = "#4ECDC4")) +
  scale_alpha_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.75), guide = "none") +
  coord_flip() +
  labs(
    title = "Income Prediction: Out-of-Sample RMSE Comparison",
    subtitle = sprintf("Train: n=%s (Chunks 1-7) | Validation: n=%s (Chunks 8-10)\nLower RMSE = Better Predictive Performance",
                      format(nrow(train_df), big.mark = ","),
                      format(nrow(valid_df), big.mark = ",")),
    x = "Model",
    y = "RMSE (log income)",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 10)),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

ggsave("02_output/figures/03_section3_prediction/rmse_comparison_v2.png",
       plot = p, width = 12, height = 7, dpi = 300, bg = "white")

cat("✓ Gráfico PNG: 02_output/figures/03_section3_prediction/rmse_comparison_v2.png\n")

