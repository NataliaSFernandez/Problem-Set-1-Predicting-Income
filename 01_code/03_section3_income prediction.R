################################################################################
# PROBLEM SET 1: Predicting Income
# Script 03: Income Prediction - Alternative Models & LOOCV Diagnostics
################################################################################
# OBJETIVO:
# Evaluar y comparar múltiples especificaciones de modelos para predecir ingresos
# laborales en Bogotá (GEIH 2018), utilizando:
#   1) Cuatro modelos base de complejidad progresiva
#   2) Cinco modelos alternativos con especificaciones económicamente motivadas
#   3) Evaluación Out-of-Sample (RMSE, mejora relativa, ranking)
#   4) Análisis LOOCV (Leave-One-Out Cross-Validation) en training
#   5) Medición de influencia en coeficientes (|| beta(-i) - beta ||²)
#   6) Diagnóstico de observaciones problemáticas (leverage, error, influencia)
#
# MOTIVACIÓN ECONÓMICA:
# La predicción de ingresos requiere capturar múltiples dimensiones del mercado
# laboral:
#   - Retornos a capital humano: edad (ciclo de vida), educación (stock de skills)
#   - Intensidad laboral: horas trabajadas y retornos no-lineales
#   - Heterogeneidad estructural: ocupación, sector, formalidad
#   - Dinámicas demográficas: brecha de género dinámica por edad
#   - Efectos de red/contexto: formalidad×educación, estrato×experiencia
#
# Este análisis permite:
#   ✓ Identificar la especificación de mejor predicción Out-of-Sample
#   ✓ Cuantificar el trade-off entre complejidad y performance
#   ✓ Diagnosticar observaciones mal predichas o influyentes
#   ✓ Evaluar robustez via LOOCV (generalización en training)
#
# DATA SPLIT (Especificación del Problem Set):
#   Train:      Chunks 1-7 (10,263 obs) → Estimación + LOOCV
#   Validation: Chunks 8-10 (4,501 obs) → Evaluación Out-of-Sample
#
# ESTRUCTUR DEL SCRIPT:
# ──────────────────────
# SECCIÓN 0: Configuración, librerías, funciones auxiliares
# SECCIÓN 1: Carga y preparación de datos (split train/validation)
# SECCIÓN 2: Construcción de variables derivadas (splines, interacciones)
# SECCIÓN 3: Estimación de 9 modelos (4 base + 5 alternativos)
# SECCIÓN 4: Predicciones en validation set (limpieza de NAs y factor levels)
# SECCIÓN 5: Evaluación Out-of-Sample (RMSE, mejoras %, ranking)
# SECCIÓN 6: Análisis LOOCV en training (mejor modelo = ALT5)
# SECCIÓN 7: Exportación completa (LaTeX, PDF, PNG, CSV)
#
# OUTPUTS:
#   Tablas:           02_output/tables/03_section3_prediction/
#   Gráficos (oos):   02_output/figures/03_section3_prediction/
#   Diagnósticos:     02_output/05_diagnostic_analysis/ (LOOCV, influencia)
#
# INPUT:
#   00_data/cleaned/data_cleaned.csv
#
################################################################################

# ==============================================================================
# SECCIÓN 0: CONFIGURACIÓN, LIBRERÍAS Y FUNCIONES AUXILIARES
# ==============================================================================

rm(list = ls())

cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                  ANÁLISIS UNIFICADO: MODELOS + DIAGNOSTICS                 ║\n")
cat("║              Modelos Alternativos para Predicción de Ingreso               ║\n")
cat("═════════════════════════════════════════════════════════════════════════════║\n")
cat("Secciones incluidas:\n")
cat("  1. Carga y preparación de datos\n")
cat("  2. Construcción de variables derivadas\n")
cat("  3. Estimación de 9 modelos (4 base + 5 alternativos)\n")
cat("  4. Predicciones en validation set\n")
cat("  5. Evaluación Out-of-Sample (RMSE, mejoras, ranking)\n")
cat("  6. Análisis LOOCV en training (Best model = ALT5)\n")
cat("  7. Exportación de resultados (tablas, gráficos, CSVs)\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASO 0.1: Instalar/cargar librerías necesarias
# ─────────────────────────────────────────────────────────────────────────────
# Librerías requeridas:
#   ggplot2:   Visualizaciones de comparación y diagnósticos
#   stargazer: Exportación de tablas a LaTeX
#   dplyr:     Data manipulation y pipelines
#   splines:   Función bs() para splines cúbicos
#   webshot:   Conversión de HTML a PNG (tablas)
#   htmltools: Utilidades HTML

packages <- c("ggplot2", "stargazer", "dplyr", "splines", "webshot", "htmltools")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# PASO 0.2: Parámetros globales y configuración de directorios
# ─────────────────────────────────────────────────────────────────────────────
# El BEST_MODEL se identifica al final de la Section 5 como el de menor RMSE
# En análisis previos, ALT5 ha mostrado mejor performance (RMSE 0.5649)

BEST_MODEL <- "ALT5"  # Modelo para diagnóstico LOOCV durante training
DATA_PATH  <- "00_data/cleaned/data_cleaned.csv"  # Path a datos limpios

# Directorios de salida
OUT_DIR_TABLES     <- "02_output/tables/03_section3_prediction"
OUT_DIR_FIGURES    <- "02_output/figures/03_section3_prediction"
OUT_DIR_DIAGNOSTIC <- "02_output/05_diagnostic_analysis"
FIG_DIR_DIAGNOSTIC <- file.path(OUT_DIR_DIAGNOSTIC, "figures")

# Crear directorios si no existen
for (dir in c(OUT_DIR_TABLES, OUT_DIR_FIGURES, FIG_DIR_DIAGNOSTIC)) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# PASO 0.3: Funciones auxiliares reutilizables
# ─────────────────────────────────────────────────────────────────────────────

# Calcula RMSE (Root Mean Square Error) - maneja NAs
calc_rmse <- function(actual, predicted) {
  mask <- !is.na(predicted) & !is.na(actual)
  if (sum(mask) == 0) return(NA_real_)
  sqrt(mean((actual[mask] - predicted[mask])^2))
}

# Calcula MAE (Mean Absolute Error) - maneja NAs
calc_mae <- function(actual, predicted) {
  mask <- !is.na(predicted) & !is.na(actual)
  if (sum(mask) == 0) return(NA_real_)
  mean(abs(actual[mask] - predicted[mask]))
}

# RMSE con información adicional (n_usado, n_total)
rmse_with_n <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  list(
    rmse = sqrt(mean((actual[ok] - predicted[ok])^2)),
    n_used = sum(ok),
    n_total = length(actual)
  )
}

# Crea columnas de splines explícitas (evita problemas con B-spline matrices)
make_spline_cols <- function(x, df = NULL, knots = NULL, degree = 3, prefix = "spl") {
  B <- if (!is.null(df)) {
    splines::bs(x, df = df, degree = degree)
  } else {
    splines::bs(x, knots = knots, degree = degree)
  }
  B <- as.data.frame(B)
  names(B) <- paste0(prefix, seq_len(ncol(B)))
  B
}

# Inversa robusta para matrices casi singulares
robust_inv <- function(A) {
  out <- tryCatch({
    cholA <- chol(A)
    chol2inv(cholA)
  }, error = function(e) NULL)
  if (!is.null(out)) return(out)
  qr.solve(A, diag(nrow(A)))
}

# Estandariza X (excepto intercepto) para medir influencia sin sesgo de escala
standardize_X <- function(X) {
  Xs <- X
  is_intercept <- apply(Xs, 2, function(col) all(abs(col - 1) < 1e-12))
  for (j in seq_len(ncol(Xs))) {
    if (is_intercept[j]) next
    sdj <- sd(Xs[, j])
    if (!is.finite(sdj) || sdj == 0) next
    Xs[, j] <- (Xs[, j] - mean(Xs[, j])) / sdj
  }
  Xs
}

# Calcula LOOCV residuals e influencia en betas
compute_loocv_and_influence <- function(X, Xs, y, resid) {
  n <- nrow(X)
  eps <- 1e-10

  # Leverage h_ii = diagonal de X(X'X)^{-1}X'
  XtX_inv <- robust_inv(crossprod(X))
  h_ii <- rowSums((X %*% XtX_inv) * X)
  denom <- pmax(1 - h_ii, eps)

  # LOOCV residual: resid_i / (1 - h_ii)
  loocv_resid <- resid / denom
  loocv_pred <- y - loocv_resid
  loocv_err <- loocv_resid

  # Influencia en betas: || beta^(-i) - beta ||²
  # Usando X estandarizada para evitar sesgo por escala
  XtX_inv_s <- robust_inv(crossprod(Xs))
  influence_sq <- numeric(n)
  for (i in seq_len(n)) {
    xi <- Xs[i, , drop = FALSE]
    delta <- -(XtX_inv_s %*% t(xi)) * (resid[i] / denom[i])
    influence_sq[i] <- sum(delta^2)
  }

  list(
    leverage = h_ii,
    loocv_pred = loocv_pred,
    loocv_err = loocv_err,
    influence_beta_sq = influence_sq
  )
}

# ==============================================================================
# SECCIÓN 1: CARGA Y PREPARACIÓN ÚNICA DE DATOS
# ==============================================================================

cat("\n================================================================================\n")
cat("SECCIÓN 1: CARGA Y PREPARACIÓN DE DATOS\n")
cat("================================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 1.1: Cargar datos CSV
# ─────────────────────────────────────────────────────────────────────────────

cat("Cargando datos desde:", DATA_PATH, "\n")
data <- read.csv(DATA_PATH)

cat(sprintf("  Total observaciones: %s\n", format(nrow(data), big.mark = ",")))
cat(sprintf("  Variables disponibles: %d\n", ncol(data)))

# ─────────────────────────────────────────────────────────────────────────────
# 1.2: Validación: Verificar que exista columna 'chunk'
# ─────────────────────────────────────────────────────────────────────────────

if (!("chunk" %in% names(data))) {
  stop("ERROR: Falta columna 'chunk'. El Problem Set exige split chunks 1-7 vs 8-10.")
}

# ─────────────────────────────────────────────────────────────────────────────
# 1.3: Split EXACTO por chunks (especificación del Problem Set)
# ─────────────────────────────────────────────────────────────────────────────

train_df <- subset(data, chunk %in% 1:7)
valid_df <- subset(data, chunk %in% 8:10)

cat(sprintf("\nDivisión por chunks:\n"))
cat(sprintf("  Train (Chunks 1-7):  %s observaciones\n", format(nrow(train_df), big.mark = ",")))
cat(sprintf("  Valid (Chunks 8-10): %s observaciones\n", format(nrow(valid_df), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# 1.4: Validación: Verificar variables críticas
# ─────────────────────────────────────────────────────────────────────────────

must_exist <- c("log_income", "age", "age_squared", "female")
missing_must <- setdiff(must_exist, names(train_df))
if (length(missing_must) > 0) {
  stop("ERROR: Faltan variables en train_df: ", paste(missing_must, collapse = ", "))
}

# ─────────────────────────────────────────────────────────────────────────────
# 1.5: Convertir variables categóricas a factores
# ─────────────────────────────────────────────────────────────────────────────

control_fac <- c("maxEducLevel", "relab", "oficio", "sizeFirm", "estrato1")
control_fac <- control_fac[control_fac %in% names(train_df)]

cat(sprintf("\nConvirtiendo %d variables categóricas a factores...\n", length(control_fac)))

for (v in control_fac) {
  train_df[[v]] <- factor(train_df[[v]])
  # Alinear niveles: valid_df tendrá solo los niveles vistos en train_df
  valid_df[[v]] <- factor(valid_df[[v]], levels = levels(train_df[[v]]))
}

cat("  ✓ Factores alineados: obs con nuevos levels en valid serán NA durante predict()\n")

# Crear variables ordinales para interacciones (NO son "años", solo códigos ordinales)
if ("maxEducLevel" %in% names(train_df)) {
  train_df$edu_ord <- as.integer(train_df$maxEducLevel)
  valid_df$edu_ord <- as.integer(valid_df$maxEducLevel)
}

if ("estrato1" %in% names(train_df)) {
  train_df$estrato_ord <- as.integer(train_df$estrato1)
  valid_df$estrato_ord <- as.integer(valid_df$estrato1)
}

cat("\n")

# ==============================================================================
# SECCIÓN 2: CONSTRUCCIÓN DE VARIABLES DERIVADAS
# ==============================================================================

cat("================================================================================\n")
cat("SECCIÓN 2: CONSTRUCCIÓN DE VARIABLES DERIVADAS\n")
cat("================================================================================\n\n")

cat("Variables construidas:\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2.1: Polinomios en edad
# ─────────────────────────────────────────────────────────────────────────────

train_df$age_cubed <- train_df$age^3
valid_df$age_cubed <- valid_df$age^3

cat("  ✓ age_cubed: para polinomios (edad³)\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2.2: Retornos decrecientes a horas (horas²)
# ─────────────────────────────────────────────────────────────────────────────

train_df$hours_squared <- train_df$totalHoursWorked^2
valid_df$hours_squared <- valid_df$totalHoursWorked^2

cat("  ✓ hours_squared: retornos decrecientes a jornada laboral\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2.3: Experiencia potencial (potExp)
# ─────────────────────────────────────────────────────────────────────────────

# maxEducLevel está codificado como 1/3/4/5/6/7 → mapeamos a años de educación
train_df$potExp <- NA_real_
valid_df$potExp <- NA_real_

if ("maxEducLevel" %in% names(train_df)) {
  train_code <- suppressWarnings(as.integer(as.character(train_df$maxEducLevel)))
  valid_code <- suppressWarnings(as.integer(as.character(valid_df$maxEducLevel)))

  edu_map <- c(
    `1` = 3,   # Primaria incompleta
    `3` = 5,   # Primaria completa
    `4` = 8,   # Secundaria incompleta
    `5` = 11,  # Secundaria completa
    `6` = 13,  # Técnico/Superior incompleto
    `7` = 16   # Profesional/Superior completo
  )

  edu_years_train <- unname(edu_map[as.character(train_code)])
  edu_years_valid <- unname(edu_map[as.character(valid_code)])

  if (!all(is.na(edu_years_train)) && "age" %in% names(train_df)) {
    train_df$potExp <- pmax(train_df$age - edu_years_train - 6, 0)
    valid_df$potExp <- pmax(valid_df$age - edu_years_valid - 6, 0)
    cat("  ✓ potExp: experiencia potencial (age - edu_years - 6)\n")
  } else {
    cat("  ⚠ potExp: No se pudo construir (problemas con codificación maxEducLevel)\n")
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 2.4: Splines cúbicos en edad (para modelos Alt 4 y Alt 5)
# ─────────────────────────────────────────────────────────────────────────────

cat("  ✓ Splines cúbicos en edad:\n")

# Splines df=5 para ALT4
spl_train4 <- make_spline_cols(train_df$age, df = 5, prefix = "age_spl4_")
spl_valid4 <- make_spline_cols(valid_df$age, df = 5, prefix = "age_spl4_")
train_df <- cbind(train_df, spl_train4)
valid_df <- cbind(valid_df, spl_valid4)
cat(sprintf("      - ALT4: %d columnas spline (df=5)\n", ncol(spl_train4)))

# Splines df=4 para ALT5
spl_train5 <- make_spline_cols(train_df$age, df = 4, prefix = "age_spl5_")
spl_valid5 <- make_spline_cols(valid_df$age, df = 4, prefix = "age_spl5_")
train_df <- cbind(train_df, spl_train5)
valid_df <- cbind(valid_df, spl_valid5)
cat(sprintf("      - ALT5: %d columnas spline (df=4)\n", ncol(spl_train5)))

# ─────────────────────────────────────────────────────────────────────────────
# 2.5: Interacciones (para ALT2, ALT4, ALT5)
# ─────────────────────────────────────────────────────────────────────────────

cat("  ✓ Interacciones económicamente significativas:\n")

# Edad × Género: brecha de género dinámica
if ("female" %in% names(train_df) && "age" %in% names(train_df)) {
  train_df$age_female_int <- train_df$age * train_df$female
  valid_df$age_female_int <- valid_df$age * valid_df$female
  cat("      - age × female (brecha dinámica)\n")
}

# Educación × Formalidad: complementariedad
if (all(c("formal", "edu_ord") %in% names(train_df)) && !all(is.na(train_df$edu_ord))) {
  train_df$formal_edu_int <- train_df$formal * train_df$edu_ord
  valid_df$formal_edu_int <- valid_df$formal * valid_df$edu_ord
  cat("      - education × formal (complementariedad)\n")
}

# Educación × Estrato: retornos heterogéneos
if (all(c("estrato_ord", "edu_ord") %in% names(train_df)) && !all(is.na(train_df$edu_ord))) {
  train_df$edu_estrato_int <- train_df$edu_ord * train_df$estrato_ord
  valid_df$edu_estrato_int <- valid_df$edu_ord * valid_df$estrato_ord
  cat("      - education × estrato (retornos heterogéneos)\n")
}

# Experiencia Potencial × Estrato: network effects
if (all(c("potExp", "estrato_ord") %in% names(train_df)) && !all(is.na(train_df$potExp))) {
  train_df$potexp_estrato_int <- train_df$potExp * train_df$estrato_ord
  valid_df$potexp_estrato_int <- valid_df$potExp * valid_df$estrato_ord
  cat("      - potExp × estrato (network effects)\n")
}

# Spline × Género (ALT4, ALT5): brecha varía con edad
if ("female" %in% names(train_df)) {
  for (nm in names(spl_train4)) {
    iname <- paste0(nm, "_x_female")
    train_df[[iname]] <- train_df[[nm]] * train_df$female
    valid_df[[iname]] <- valid_df[[nm]] * valid_df$female
  }
  cat("      - spline(age) × female (ALT4: brecha varía con edad)\n")
}

cat("\n")

# ==============================================================================
# SECCIÓN 3: ESTIMACIÓN DE MODELOS
# ==============================================================================

cat("================================================================================\n")
cat("SECCIÓN 3: ESTIMACIÓN DE 9 MODELOS (4 BASE + 5 ALTERNATIVOS)\n")
cat("================================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3.1: MODELOS BASE (para comparación)
# ─────────────────────────────────────────────────────────────────────────────

cat("MODELOS BASE:\n")
cat("─────────────\n\n")

# Base 0: Raw gender gap (sin controles)
base_model0 <- lm(log_income ~ female, data = train_df)
base0_r2 <- summary(base_model0)$r.squared
cat(sprintf("Base 0: Female only (raw gap)\n  R² Train: %.4f | N params: %d\n\n",
            base0_r2, length(coef(base_model0))))

# Base 1: Section 1 unconditional (edad + edad²)
base_model1 <- lm(log_income ~ age + age_squared, data = train_df)
base1_r2 <- summary(base_model1)$r.squared
cat(sprintf("Base 1: Age + age² (Section 1 uncond)\n  R² Train: %.4f | N params: %d\n\n",
            base1_r2, length(coef(base_model1))))

# Base 2: Section 1 conditional (age + age² + horas + relab)
base_model2 <- lm(log_income ~ age + age_squared + totalHoursWorked + factor(relab),
                  data = train_df)
base2_r2 <- summary(base_model2)$r.squared
cat(sprintf("Base 2: Age + age² + hours + relab (Section 1 cond)\n  R² Train: %.4f | N params: %d\n\n",
            base2_r2, length(coef(base_model2))))

# Base 3: Section 2 conditional (female + controles ampliados)
control_num <- c("age", "age_squared", "totalHoursWorked", "formal", "p6426", "p6240")
control_num <- control_num[control_num %in% names(train_df)]

rhs_base3 <- c("female", control_num, paste0("factor(", control_fac, ")"))
rhs_base3 <- unique(rhs_base3)

f_base3 <- as.formula(paste("log_income ~", paste(rhs_base3, collapse = " + ")))
base_model3 <- lm(f_base3, data = train_df)
base3_r2 <- summary(base_model3)$r.squared
cat(sprintf("Base 3: Female + full controls (Section 2 cond)\n  R² Train: %.4f | N params: %d\n\n",
            base3_r2, length(coef(base_model3))))

# ─────────────────────────────────────────────────────────────────────────────
# 3.2: MODELOS ALTERNATIVOS (mejoras propuestas para mejor predicción)
# ─────────────────────────────────────────────────────────────────────────────

cat("\nMODELOS ALTERNATIVOS:\n")
cat("────────────────────\n\n")

# ALT 1: Polinomios cúbicos + controles extendidos
cat("ALT 1: Cubic polynomials + extended controls\n")
cat("  Racionalidad: Captura curvatura edad-ingreso sin asumir forma paramétrica\n")
alt_model1 <- lm(log_income ~ age + age_squared + age_cubed +
                   totalHoursWorked + formal + p6426 + p6240 +
                   factor(maxEducLevel) + factor(relab) + factor(oficio) +
                   factor(sizeFirm) + factor(estrato1),
                 data = train_df)
alt1_r2 <- summary(alt_model1)$r.squared
cat(sprintf("  R² Train: %.4f | N params: %d\n\n", alt1_r2, length(coef(alt_model1))))

# ALT 2: Interacciones clave
cat("ALT 2: Key interactions (age×female, edu×formal, edu×estrato)\n")
cat("  Racionalidad: Permite retornos heterogéneos por grupo demográfico\n")
alt_model2 <- lm(log_income ~ age + age_squared + female + totalHoursWorked +
                   formal + p6426 + p6240 +
                   factor(maxEducLevel) + factor(relab) + factor(sizeFirm) +
                   factor(estrato1) + age_female_int + formal_edu_int +
                   edu_estrato_int,
                 data = train_df)
alt2_r2 <- summary(alt_model2)$r.squared
cat(sprintf("  R² Train: %.4f | N params: %d\n\n", alt2_r2, length(coef(alt_model2))))

# ALT 3: Efectos fijos ocupacionales + variables nuevas
cat("ALT 3: Occupational Fixed Effects + new variables (p6240, estrato, p6426, potExp)\n")
cat("  Racionalidad: Heterogeneidad estructural laboral + canales no observados\n")
alt_model3 <- lm(log_income ~ age + age_squared + female + totalHoursWorked +
                   formal + p6426 + p6240 + potExp +
                   factor(maxEducLevel) + factor(relab) + factor(oficio) +
                   factor(sizeFirm) + factor(estrato1),
                 data = train_df)
alt3_r2 <- summary(alt_model3)$r.squared
cat(sprintf("  R² Train: %.4f | N params: %d\n\n", alt3_r2, length(coef(alt_model3))))

# ALT 4: Splines cúbicos + interacciones múltiples
cat("ALT 4: Cubic splines (df=5) + multiple interactions\n")
cat("  Racionalidad: Máxima flexibilidad local en perfil edad-ingreso\n")
terms_alt4 <- c(
  "female", names(spl_train4), paste0(names(spl_train4), "_x_female"),
  "totalHoursWorked", "formal", "p6426", "p6240",
  "factor(maxEducLevel)", "factor(relab)"
)
terms_alt4_ok <- sapply(terms_alt4, function(t) {
  if (grepl("^factor\\(", t)) {
    v <- gsub("^factor\\(|\\)$", "", t)
    v %in% names(train_df)
  } else {
    t %in% names(train_df) && !all(is.na(train_df[[t]]))
  }
})
terms_alt4 <- terms_alt4[terms_alt4_ok]

f_alt4 <- as.formula(paste("log_income ~", paste(terms_alt4, collapse = " + ")))
alt_model4 <- lm(f_alt4, data = train_df)
alt4_r2 <- summary(alt_model4)$r.squared
cat(sprintf("  R² Train: %.4f | N params: %d\n\n", alt4_r2, length(coef(alt_model4))))

# ALT 5: MODELO INTEGRADO (Splines + Interacciones + FE + variables nuevas)
cat("ALT 5: INTEGRATED (Splines df=4 + interactions + FE + new vars)\n")
cat("  Racionalidad: Síntesis de TODAS las consideraciones económicas\n")
cat("  Combinaciones:\n")
cat("    - Flexibilidad edad-ingreso (splines sin forma paramétrica)\n")
cat("    - Retornos heterogéneos (edad×género, educación×formalidad, etc.)\n")
cat("    - Controles estructurales (FE ocupación, estrato)\n")
cat("    - Variables nuevas (p6240, p6426, estrato1, potExp)\n")

terms_alt5 <- c(
  "female", names(spl_train5),
  "totalHoursWorked", "hours_squared",
  "female_edu_int", "formal_edu_int", "potexp_estrato_int",
  "formal", "p6426", "p6240",
  "factor(oficio)", "factor(sizeFirm)", "factor(estrato1)",
  "factor(maxEducLevel)", "factor(relab)"
)
terms_alt5_ok <- sapply(terms_alt5, function(t) {
  if (grepl("^factor\\(", t)) {
    v <- gsub("^factor\\(|\\)$", "", t)
    v %in% names(train_df)
  } else {
    t %in% names(train_df) && !all(is.na(train_df[[t]]))
  }
})
terms_alt5 <- terms_alt5[terms_alt5_ok]

f_alt5 <- as.formula(paste("log_income ~", paste(terms_alt5, collapse = " + ")))
alt_model5 <- lm(f_alt5, data = train_df)
alt5_r2 <- summary(alt_model5)$r.squared
cat(sprintf("  R² Train: %.4f | N params: %d\n\n", alt5_r2, length(coef(alt_model5))))

cat("\n")

# ==============================================================================
# SECCIÓN 4: PREDICCIONES EN VALIDATION SET
# ==============================================================================

cat("================================================================================\n")
cat("SECCIÓN 4: PREDICCIONES EN VALIDATION SET\n")
cat("================================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.1: Limpiar validation set (remover NAs en variables críticas)
# ─────────────────────────────────────────────────────────────────────────────

cat("Limpiando validation set de observaciones con valores faltantes...\n")

vars_critical <- c("log_income", "age", "female", "maxEducLevel", "formal",
                   "p6240", "p6426", "oficio", "estrato1", "totalHoursWorked",
                   "age_squared", "relab", "sizeFirm")

valid_idx <- complete.cases(valid_df[, vars_critical])
valid_df_clean <- valid_df[valid_idx, ]

cat(sprintf("  Original:  %d obs\n", nrow(valid_df)))
cat(sprintf("  Limpio:    %d obs\n", nrow(valid_df_clean)))
cat(sprintf("  Removidas: %d obs (%.2f%% por NAs)\n",
            nrow(valid_df) - nrow(valid_df_clean),
            100 * (nrow(valid_df) - nrow(valid_df_clean)) / nrow(valid_df)))

# ─────────────────────────────────────────────────────────────────────────────
# 4.2: Remover observaciones con factor levels no vistos en training
# ─────────────────────────────────────────────────────────────────────────────

cat("\nFiltrando observaciones con factor levels no vistos en training...\n")

# Extraer niveles vistos en training para cada factor
train_levels <- list()
for (v in control_fac) {
  if (v %in% names(train_df)) {
    train_levels[[v]] <- sort(unique(as.character(train_df[[v]][!is.na(train_df[[v]])])))
  }
}

# Crear máscara de observaciones válidas
valid_mask <- rep(TRUE, nrow(valid_df_clean))
for (v in control_fac) {
  if (v %in% names(valid_df_clean) && v %in% names(train_levels)) {
    valid_mask <- valid_mask & (as.character(valid_df_clean[[v]]) %in% train_levels[[v]])
  }
}

valid_df_clean_filtered <- valid_df_clean[valid_mask, ]

cat(sprintf("  Antes:     %d obs\n", nrow(valid_df_clean)))
cat(sprintf("  Después:   %d obs\n", nrow(valid_df_clean_filtered)))
cat(sprintf("  Removidas: %d obs (%.2f%% por factor levels nuevos)\n",
            nrow(valid_df_clean) - nrow(valid_df_clean_filtered),
            100 * (nrow(valid_df_clean) - nrow(valid_df_clean_filtered)) / nrow(valid_df_clean)))

valid_df_clean <- valid_df_clean_filtered

# ─────────────────────────────────────────────────────────────────────────────
# 4.3: Generar predicciones para todos los modelos
# ─────────────────────────────────────────────────────────────────────────────

cat(sprintf("\nGenerando predicciones para 9 modelos (n = %d)...\n", nrow(valid_df_clean)))

pred_base0 <- predict(base_model0, newdata = valid_df_clean, na.action = na.exclude)
pred_base1 <- predict(base_model1, newdata = valid_df_clean, na.action = na.exclude)
pred_base2 <- predict(base_model2, newdata = valid_df_clean, na.action = na.exclude)
pred_base3 <- predict(base_model3, newdata = valid_df_clean, na.action = na.exclude)
pred_alt1  <- predict(alt_model1, newdata = valid_df_clean, na.action = na.exclude)
pred_alt2  <- predict(alt_model2, newdata = valid_df_clean, na.action = na.exclude)
pred_alt3  <- predict(alt_model3, newdata = valid_df_clean, na.action = na.exclude)
pred_alt4  <- predict(alt_model4, newdata = valid_df_clean, na.action = na.exclude)
pred_alt5  <- predict(alt_model5, newdata = valid_df_clean, na.action = na.exclude)

# ─────────────────────────────────────────────────────────────────────────────
# 4.4: VALIDACIÓN: Verificar alineación de predicciones
# ─────────────────────────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("VALIDACIÓN: ALINEACIÓN DE PREDICCIONES\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

all_preds <- list(pred_base0, pred_base1, pred_base2, pred_base3,
                  pred_alt1, pred_alt2, pred_alt3, pred_alt4, pred_alt5)
pred_names <- c("Base 0", "Base 1", "Base 2", "Base 3",
                "Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5")

cat("Longitud de predicciones:\n")
for (i in seq_along(all_preds)) {
  n_pred <- length(all_preds[[i]])
  n_na <- sum(is.na(all_preds[[i]]))
  status <- if (n_pred == nrow(valid_df_clean)) "✓" else "✗"
  cat(sprintf("  %s  %s: %d predicciones, %d NAs\n",
              status, pred_names[i], n_pred, n_na))
}

# Verificar que NO hay desalineación
misalignment <- any(sapply(all_preds, length) != nrow(valid_df_clean))
if (!misalignment) {
  cat("\n✓ RESULTADO: Todas las predicciones están correctamente alineadas\n")
} else {
  cat("\n✗ RESULTADO: PROBLEMA DETECTADO en alineación\n")
}

cat("\n")

# ==============================================================================
# SECCIÓN 5: EVALUACIÓN OUT-OF-SAMPLE (RMSE, MEJORAS, RANKING)
# ==============================================================================

cat("================================================================================\n")
cat("SECCIÓN 5: EVALUACIÓN OUT-OF-SAMPLE (RMSE Y MEJORAS)\n")
cat("================================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 5.1: Calcular métricas para cada modelo
# ─────────────────────────────────────────────────────────────────────────────

base0_out <- rmse_with_n(valid_df_clean$log_income, pred_base0)
base1_out <- rmse_with_n(valid_df_clean$log_income, pred_base1)
base2_out <- rmse_with_n(valid_df_clean$log_income, pred_base2)
base3_out <- rmse_with_n(valid_df_clean$log_income, pred_base3)

alt1_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt1)
alt2_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt2)
alt3_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt3)
alt4_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt4)
alt5_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt5)

rmse_base0 <- base0_out$rmse
rmse_base1 <- base1_out$rmse
rmse_base2 <- base2_out$rmse
rmse_base3 <- base3_out$rmse
rmse_alt1  <- alt1_out$rmse
rmse_alt2  <- alt2_out$rmse
rmse_alt3  <- alt3_out$rmse
rmse_alt4  <- alt4_out$rmse
rmse_alt5  <- alt5_out$rmse

# ─────────────────────────────────────────────────────────────────────────────
# 5.2: Calcular mejoras relativas respecto a baseline (Base 1)
# ─────────────────────────────────────────────────────────────────────────────

baseline_rmse <- rmse_base1
all_rmses <- c(rmse_base0, rmse_base1, rmse_base2, rmse_base3,
               rmse_alt1, rmse_alt2, rmse_alt3, rmse_alt4, rmse_alt5)

improvements <- 100 * (baseline_rmse - all_rmses) / baseline_rmse

# ─────────────────────────────────────────────────────────────────────────────
# 5.3: Mostrar resultados
# ─────────────────────────────────────────────────────────────────────────────

cat(sprintf("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(sprintf("║ RESULTADOS: RMSE EN VALIDATION SET (%d observaciones limpias)          ║\n",
            nrow(valid_df_clean)))
cat(sprintf("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

cat("MODELOS BASE (para comparación):\n")
cat("─────────────────────────────────\n")
cat(sprintf("  Base 0 (female only): RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_base0, base0_r2, improvements[1]))
cat(sprintf("  Base 1 (simple):      RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_base1, base1_r2, improvements[2]))
cat(sprintf("  Base 2 (básico):      RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_base2, base2_r2, improvements[3]))
cat(sprintf("  Base 3 (control):     RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_base3, base3_r2, improvements[4]))

cat("\nMODELOS ALTERNATIVOS:\n")
cat("────────────────────\n")
cat(sprintf("  Alt 1 (polynomial):   RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_alt1, alt1_r2, improvements[5]))
cat(sprintf("  Alt 2 (interactions): RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_alt2, alt2_r2, improvements[6]))
cat(sprintf("  Alt 3 (extended):     RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_alt3, alt3_r2, improvements[7]))
cat(sprintf("  Alt 4 (spline):       RMSE = %.4f | R² = %.4f | Mejora: %+.2f%%\n",
            rmse_alt4, alt4_r2, improvements[8]))
cat(sprintf("  Alt 5 (INTEGRADO):    RMSE = %.4f | R² = %.4f | Mejora: %+.2f%% ⭐\n",
            rmse_alt5, alt5_r2, improvements[9]))

# ─────────────────────────────────────────────────────────────────────────────
# 5.4: Ranking final
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\nRANKING FINAL (menor RMSE = mejor):\n")
cat("────────────────────────────────────\n")

all_names <- c("Base 0", "Base 1", "Base 2", "Base 3",
               "Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5")
ranking <- order(all_rmses)

for (i in seq_along(ranking)) {
  idx <- ranking[i]
  medal <- if (i == 1) "🥇" else if (i == 2) "🥈" else if (i == 3) "🥉" else "  "
  cat(sprintf("%d. %s  %s: RMSE = %.4f | Mejora = %+.2f%%\n",
              i, medal, all_names[idx], all_rmses[idx], improvements[idx]))
}

cat("\n")

# ==============================================================================
# SECCIÓN 6: ANÁLISIS LOOCV EN TRAINING (Mejor modelo = ALT5)
# ==============================================================================

cat("================================================================================\n")
cat("SECCIÓN 6: ANÁLISIS LOOCV EN TRAINING (MEJOR MODELO = ALT5)\n")
cat("================================================================================\n\n")

cat(sprintf("Calculando diagnóstico LOOCV para el mejor modelo: %s\n\n", BEST_MODEL))

# ─────────────────────────────────────────────────────────────────────────────
# 6.1: Preparar datos de entrenamiento para LOOCV (remover NAs)
# ─────────────────────────────────────────────────────────────────────────────

cat("Limpiando training set (remover NAs)...\n")

# Construir formulario ALT5 y usar model.frame para obtener muestra limpia
mf_train <- model.frame(f_alt5, data = train_df, na.action = na.omit)
train_used <- train_df[as.integer(rownames(mf_train)), , drop = FALSE]

cat(sprintf("  Original:  %d obs\n", nrow(train_df)))
cat(sprintf("  Limpio:    %d obs\n", nrow(train_used)))
cat(sprintf("  Removidas: %d obs (%.2f%% por NAs)\n\n",
            nrow(train_df) - nrow(train_used),
            100 * (nrow(train_df) - nrow(train_used)) / nrow(train_df)))

# ─────────────────────────────────────────────────────────────────────────────
# 6.2: Re-estimar mejor modelo en datos limpios
# ─────────────────────────────────────────────────────────────────────────────

cat("Re-estimando modelo ALT5 en training limpio...\n\n")

alt5_model_for_loocv <- lm(f_alt5, data = train_used)

# ─────────────────────────────────────────────────────────────────────────────
# 6.3: Calcular LOOCV e influencia
# ─────────────────────────────────────────────────────────────────────────────

X  <- model.matrix(alt5_model_for_loocv, data = train_used)
y  <- train_used$log_income
res <- resid(alt5_model_for_loocv)

Xs <- standardize_X(X)

loocv_inf <- compute_loocv_and_influence(X = X, Xs = Xs, y = y, resid = res)
loocv_pred <- loocv_inf$loocv_pred
loocv_err  <- loocv_inf$loocv_err
lev        <- loocv_inf$leverage
infl_sq    <- loocv_inf$influence_beta_sq

# ─────────────────────────────────────────────────────────────────────────────
# 6.4: Calcular RMSE LOOCV vs Validation RMSE
# ─────────────────────────────────────────────────────────────────────────────

rmse_loocv <- calc_rmse(y, loocv_pred)
mae_loocv  <- calc_mae(y, loocv_pred)

# Validation RMSE (ALT5)
valid_pred_alt5 <- predict(alt5_model_for_loocv, newdata = valid_df_clean)
rmse_valid_alt5 <- calc_rmse(valid_df_clean$log_income, valid_pred_alt5)
mae_valid_alt5  <- calc_mae(valid_df_clean$log_income, valid_pred_alt5)

cat(sprintf("LOOCV (en training, n=%d):\n", nrow(train_used)))
cat(sprintf("  RMSE: %.4f | MAE: %.4f\n\n", rmse_loocv, mae_loocv))

cat(sprintf("Validation (en validation, n=%d):\n", nrow(valid_df_clean)))
cat(sprintf("  RMSE: %.4f | MAE: %.4f\n\n", rmse_valid_alt5, mae_valid_alt5))

cat(sprintf("Gap (LOOCV RMSE - Validation RMSE): %+.4f\n\n",
            rmse_loocv - rmse_valid_alt5))

# ─────────────────────────────────────────────────────────────────────────────
# 6.5: Crear tabla diagnóstica por observación
# ─────────────────────────────────────────────────────────────────────────────

diag_df <- data.frame(
  row_id = as.integer(rownames(train_used)),
  chunk = if ("chunk" %in% names(train_used)) train_used$chunk else NA,
  y = y,
  resid = res,
  leverage = lev,
  loocv_pred = loocv_pred,
  loocv_err = loocv_err,
  abs_loocv_err = abs(loocv_err),
  influence_beta_sq = infl_sq
)

# Calcular umbrales
n <- nrow(diag_df)
k <- ncol(X)
lev_thresh <- 2 * k / n
err_thresh <- quantile(diag_df$abs_loocv_err, 0.95, na.rm = TRUE)
inf_thresh <- quantile(diag_df$influence_beta_sq, 0.99, na.rm = TRUE)

# Marcar observaciones problemáticas
diag_df <- diag_df %>%
  mutate(
    high_error = abs_loocv_err >= err_thresh,
    high_leverage = leverage >= lev_thresh,
    high_influence = influence_beta_sq >= inf_thresh,
    problematic = high_error | high_leverage | high_influence
  )

cat("Umbrales para identificar observaciones problemáticas:\n")
cat(sprintf("  Leverage alto:      h_ii >= %.4f (regla: 2k/n)\n", lev_thresh))
cat(sprintf("  Error LOOCV alto:   |e_i| >= %.4f (percentil 95)\n", err_thresh))
cat(sprintf("  Influencia alta:    ||beta(-i) - beta||² >= %.6f (percentil 99)\n\n",
            inf_thresh))

n_problematic <- sum(diag_df$problematic)
cat(sprintf("Observaciones problemáticas detectadas: %d (%.2f%%)\n\n",
            n_problematic, 100 * n_problematic / nrow(diag_df)))

# ─────────────────────────────────────────────────────────────────────────────
# 6.6: VALIDACIÓN: Detectar bugs silenciosos
# ─────────────────────────────────────────────────────────────────────────────

cat("════════════════════════════════════════════════════════════════════════════\n")
cat("VALIDACIÓN: CHEQUEOS DE SANIDAD (LOOCV)\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

# Chequeo 1: NaNs en LOOCV errors
if (any(!is.finite(loocv_err))) {
  cat("⚠  WARNING: LOOCV errors contiene valores no finitos\n")
  cat(sprintf("    Causa probable: leverage cercano a 1\n"))
  cat(sprintf("    Max leverage: %.4f\n\n", max(lev, na.rm = TRUE)))
} else {
  cat("✓  No hay valores no finitos en LOOCV errors\n\n")
}

# Chequeo 2: NAs en predicciones validation para ALT5
na_rate_valid_alt5 <- mean(is.na(valid_pred_alt5))
if (na_rate_valid_alt5 > 0) {
  cat(sprintf("⚠  WARNING: ALT5 validation predictions contienen NAs (%.2f%%)\n",
              100 * na_rate_valid_alt5))
  cat("    Causas: factor levels nuevos o covariables faltantes\n\n")
} else {
  cat("✓  No hay NAs en predicciones validation de ALT5\n\n")
}

# Chequeo 3: Colinealidad (si existe)
if (any(!is.finite(diag_df$influence_beta_sq))) {
  cat("⚠  WARNING: Influencia contiene valores no finitos\n")
  cat("    Causa probable: colinealidad o matriz X singular\n\n")
} else {
  cat("✓  Influencia computada correctamente para todas las obs\n\n")
}

cat("\n")

# ==============================================================================
# SECCIÓN 7: EXPORTACIÓN DE RESULTADOS
# ==============================================================================

cat("================================================================================\n")
cat("SECCIÓN 7: EXPORTACIÓN DE RESULTADOS\n")
cat("================================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 7.1: Tabla comparativa de modelos (Out-of-Sample)
# ─────────────────────────────────────────────────────────────────────────────

cat("Generando tabla comparativa de modelos...\n")

results_comparison <- data.frame(
  Model = c("Base 0: female", "Base 1: age+age²", "Base 2: Base1+hours+relab",
            "Base 3: female+controls", "Alt 1: poly³", "Alt 2: interactions",
            "Alt 3: FE+new vars", "Alt 4: splines(df=5)", "Alt 5: integrated"),
  Type = c(rep("Base", 4), rep("Alternative", 5)),
  RMSE_Valid = c(rmse_base0, rmse_base1, rmse_base2, rmse_base3,
                 rmse_alt1, rmse_alt2, rmse_alt3, rmse_alt4, rmse_alt5),
  N_valid_used = c(base0_out$n_used, base1_out$n_used, base2_out$n_used, base3_out$n_used,
                   alt1_out$n_used, alt2_out$n_used, alt3_out$n_used, alt4_out$n_used,
                   alt5_out$n_used),
  R2_Train = c(base0_r2, base1_r2, base2_r2, base3_r2,
               alt1_r2, alt2_r2, alt3_r2, alt4_r2, alt5_r2),
  Improvement_Pct = improvements,
  Rank = rank(all_rmses)
)

# Exportar tabla LaTeX
invisible(capture.output(
  stargazer(results_comparison,
            type = "latex",
            title = "Income Prediction: Comparison of Alternative Models",
            label = "tab:alternative_models_unified",
            summary = FALSE,
            rownames = FALSE,
            digits = 4,
            out = file.path(OUT_DIR_TABLES, "model_comparison.tex"))
))

cat(sprintf("  ✓ Tabla LaTeX: %s\n",
            file.path(OUT_DIR_TABLES, "model_comparison.tex")))

# Compilar LaTeX a PDF
cat("Compilando tabla a PDF...\n")
tex_file <- file.path(OUT_DIR_TABLES, "model_comparison.tex")
pdf_file <- file.path(OUT_DIR_TABLES, "model_comparison.pdf")

# Crear documento LaTeX completo con preámbulo
latex_complete <- sprintf(
  "\\documentclass[12pt,a4paper]{article}\n\\usepackage[margin=1in]{geometry}\n\\usepackage{booktabs}\n\\usepackage{amsmath}\n\\begin{document}\n\\input{%s}\n\\end{document}",
  basename(tex_file)
)

# Guardar archivo LaTeX completo
tex_complete_file <- file.path(OUT_DIR_TABLES, "model_comparison_complete.tex")
writeLines(latex_complete, con = tex_complete_file)

# Compilar a PDF
old_wd <- getwd()
setwd(OUT_DIR_TABLES)
system(sprintf("pdflatex -interaction=nonstopmode %s > /dev/null 2>&1", 
               basename(tex_complete_file)), ignore.stdout = TRUE)
setwd(old_wd)

# Renombrar y limpiar
if (file.exists(file.path(OUT_DIR_TABLES, "model_comparison_complete.pdf"))) {
  file.rename(file.path(OUT_DIR_TABLES, "model_comparison_complete.pdf"), pdf_file)
  cat(sprintf("  ✓ Tabla PDF: %s\n", pdf_file))
  # Limpiar archivos temporales
  unlink(c(tex_complete_file, 
           file.path(OUT_DIR_TABLES, "model_comparison_complete.log"),
           file.path(OUT_DIR_TABLES, "model_comparison_complete.aux")))
}

# ─────────────────────────────────────────────────────────────────────────────
# 7.2: Gráfico comparativo de RMSE (Out-of-Sample)
# ─────────────────────────────────────────────────────────────────────────────

cat("Generando gráfico comparativo de RMSE...\n")

plot_data <- data.frame(
  Model = factor(all_names, levels = all_names),
  RMSE = all_rmses,
  Type = c(rep("Base", 4), rep("Alternative", 5)),
  Rank = rank(all_rmses)
)

p <- ggplot(plot_data, aes(x = reorder(Model, RMSE), y = RMSE, fill = Type)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.4f\n(%.1f%%)", RMSE,
                                100 * (baseline_rmse - RMSE) / baseline_rmse)),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  scale_fill_manual(values = c(Base = "#FF6B6B", Alternative = "#4ECDC4")) +
  coord_flip() +
  labs(
    title = "Income Prediction: Out-of-Sample RMSE Comparison",
    subtitle = sprintf("Train: n=%s (Chunks 1-7) | Validation: n=%d (Chunks 8-10, cleaned)",
                      format(nrow(train_df), big.mark = ","),
                      nrow(valid_df_clean)),
    x = "Model",
    y = "RMSE (log income)",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 10)),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "top"
  )

ggsave(file.path(OUT_DIR_FIGURES, "rmse_comparison.png"),
       plot = p, width = 10, height = 6, dpi = 300, bg = "white")

cat(sprintf("  ✓ Gráfico PNG: %s\n",
            file.path(OUT_DIR_FIGURES, "rmse_comparison.png")))

# ─────────────────────────────────────────────────────────────────────────────
# 7.3: CSVs de diagnóstico LOOCV
# ─────────────────────────────────────────────────────────────────────────────

cat("Generando CSVs de diagnóstico LOOCV...\n")

# Resumen LOOCV vs Validation
summary_loocv <- data.frame(
  best_model = BEST_MODEL,
  n_train_total = nrow(train_df),
  n_train_used = nrow(train_used),
  n_valid_total = nrow(valid_df),
  n_valid_clean = nrow(valid_df_clean),
  rmse_loocv_train = rmse_loocv,
  mae_loocv_train = mae_loocv,
  rmse_validation = rmse_valid_alt5,
  mae_validation = mae_valid_alt5,
  gap_rmse = rmse_loocv - rmse_valid_alt5
)

write.csv(summary_loocv,
          file.path(OUT_DIR_DIAGNOSTIC, "loocv_vs_validation_summary.csv"),
          row.names = FALSE)

cat(sprintf("  ✓ %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "loocv_vs_validation_summary.csv")))

# Top 100 observaciones con mayor |LOOCV error|
top_err <- diag_df %>% arrange(desc(abs_loocv_err)) %>% head(100)
write.csv(top_err,
          file.path(OUT_DIR_DIAGNOSTIC, "top100_loocv_errors.csv"),
          row.names = FALSE)

cat(sprintf("  ✓ %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "top100_loocv_errors.csv")))

# Top 100 observaciones con mayor influencia
top_inf <- diag_df %>% arrange(desc(influence_beta_sq)) %>% head(100)
write.csv(top_inf,
          file.path(OUT_DIR_DIAGNOSTIC, "top100_influence_beta_sq.csv"),
          row.names = FALSE)

cat(sprintf("  ✓ %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "top100_influence_beta_sq.csv")))

# Tabla completa de diagnóstico (todas las obs training)
write.csv(diag_df,
          file.path(OUT_DIR_DIAGNOSTIC, "loocv_influence_all_train.csv"),
          row.names = FALSE)

cat(sprintf("  ✓ %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "loocv_influence_all_train.csv")))

# ─────────────────────────────────────────────────────────────────────────────
# 7.4: Gráficos de diagnóstico LOOCV
# ─────────────────────────────────────────────────────────────────────────────

cat("Generando gráficos de diagnóstico LOOCV...\n")

# Histograma LOOCV errors
p1 <- ggplot(diag_df, aes(x = loocv_err)) +
  geom_histogram(bins = 50, fill = "#FF6B6B", alpha = 0.7) +
  labs(
    title = "LOOCV Errors Distribution (Training)",
    x = "LOOCV error (y_i - ŷ_i^{(-i)})",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(file.path(FIG_DIR_DIAGNOSTIC, "01_loocv_error_hist.png"),
       p1, width = 10, height = 6, dpi = 300, bg = "white")

# LOOCV error vs Leverage
p2 <- ggplot(diag_df, aes(x = leverage, y = loocv_err)) +
  geom_point(alpha = 0.5, color = "#4ECDC4") +
  geom_vline(xintercept = lev_thresh, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
  labs(
    title = "LOOCV Error vs Leverage",
    subtitle = "Red dashed line: leverage threshold (2k/n)",
    x = "Leverage (h_ii)",
    y = "LOOCV error"
  ) +
  theme_minimal()

ggsave(file.path(FIG_DIR_DIAGNOSTIC, "02_loocv_leverage.png"),
       p2, width = 10, height = 6, dpi = 300, bg = "white")

# Influencia vs |LOOCV error|
p3 <- ggplot(diag_df, aes(x = abs_loocv_err, y = influence_beta_sq)) +
  geom_point(alpha = 0.5, color = "#95DE64") +
  geom_vline(xintercept = err_thresh, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = inf_thresh, linetype = "dashed", color = "red") +
  labs(
    title = "Influence in Coefficients vs Difficulty",
    subtitle = "Influence = ||beta(-i) - beta||² (on standardized X)",
    x = "|LOOCV error|",
    y = "Influence in betas"
  ) +
  theme_minimal()

ggsave(file.path(FIG_DIR_DIAGNOSTIC, "03_influence_vs_error.png"),
       p3, width = 10, height = 6, dpi = 300, bg = "white")

# Top 50 observaciones más problemáticas
top50 <- diag_df %>% arrange(desc(abs_loocv_err)) %>% head(50)
p4 <- ggplot(top50, aes(x = reorder(as.factor(row_id), -abs_loocv_err),
                        y = abs_loocv_err)) +
  geom_col(fill = "#FFB347") +
  coord_flip() +
  labs(
    title = "Top 50 Observations by LOOCV Difficulty",
    x = "Observation ID (in training)",
    y = "|LOOCV error|"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(FIG_DIR_DIAGNOSTIC, "04_top50_difficult.png"),
       p4, width = 10, height = 8, dpi = 300, bg = "white")

cat(sprintf("  ✓ 4 gráficos PNG en %s\n", FIG_DIR_DIAGNOSTIC))

cat("\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                     ✅ ANÁLISIS COMPLETADO EXITOSAMENTE                    ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

cat("RESUMEN DE OUTPUTSUMM:\n\n")

cat("📊 TABLAS COMPARATIVAS (Out-of-Sample):\n")
cat(sprintf("   %s\n", file.path(OUT_DIR_TABLES, "model_comparison.tex")))

cat("\n📈 GRÁFICOS (Out-of-Sample):\n")
cat(sprintf("   %s\n", file.path(OUT_DIR_FIGURES, "rmse_comparison.png")))

cat("\n📋 DIAGNÓSTICO LOOCV (Training Set):\n")
cat(sprintf("   CSV Summary:    %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "loocv_vs_validation_summary.csv")))
cat(sprintf("   Top 100 Errors: %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "top100_loocv_errors.csv")))
cat(sprintf("   Top 100 Influe: %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "top100_influence_beta_sq.csv")))
cat(sprintf("   Full Dataset:   %s\n",
            file.path(OUT_DIR_DIAGNOSTIC, "loocv_influence_all_train.csv")))

cat(sprintf("\n   Gráficos (4 PNG):\n"))
cat(sprintf("      %s\n", file.path(FIG_DIR_DIAGNOSTIC, "01_loocv_error_hist.png")))
cat(sprintf("      %s\n", file.path(FIG_DIR_DIAGNOSTIC, "02_loocv_leverage.png")))
cat(sprintf("      %s\n", file.path(FIG_DIR_DIAGNOSTIC, "03_influence_vs_error.png")))
cat(sprintf("      %s\n", file.path(FIG_DIR_DIAGNOSTIC, "04_top50_difficult.png")))

cat("\n\n🏆 MEJOR MODELO:\n")
cat(sprintf("   ALT 5 (INTEGRATED)\n"))
cat(sprintf("   RMSE = %.4f (Out-of-Sample)\n", rmse_alt5))
cat(sprintf("   Mejora vs Baseline: %+.2f%%\n", improvements[9]))

cat("\n\n")
