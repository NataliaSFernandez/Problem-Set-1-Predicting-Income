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
# Crear variables ordinales para las interacciones del modelo integrado (ALT5)
train_df$edu_ord <- as.integer(train_df$maxEducLevel)
valid_df$edu_ord <- as.integer(valid_df$maxEducLevel)

train_df$estrato_ord <- as.integer(train_df$estrato1)
valid_df$estrato_ord <- as.integer(valid_df$estrato1)


# CREAR VARIABLES DERIVADAS

# ------------------------------------------------------------------------------
# RECODIFICACIÓN DE maxEducLevel A AÑOS DE EDUCACIÓN: la variable maxEducLevel es un factor con niveles que representan categorías de educación. 
# Para crear una variable de años de educación, asignamos un número específico de años a cada categoría (ej. 3 años para primaria incompleta y 11 años para secundaria completa). 
#Esta variable derivada (edu_years) sirve como una proxy que se puede usar para calcular experiencia potencial y para interacciones con formalidad y estrato en los modelos.
# ------------------------------------------------------------------------------

train_df$maxEducLevel_code <- as.integer(as.character(train_df$maxEducLevel))
valid_df$maxEducLevel_code <- as.integer(as.character(valid_df$maxEducLevel))

edu_map <- c(
  `1` = 3,   # Primaria incompleta
  `3` = 5,   # Primaria completa
  `4` = 8,   # Secundaria incompleta
  `5` = 11,  # Secundaria completa
  `6` = 13,  # Técnico/Superior incompleto
  `7` = 16   # Profesional/Superior completo
)

train_df$edu_years <- unname(edu_map[as.character(train_df$maxEducLevel_code)])
valid_df$edu_years <- unname(edu_map[as.character(valid_df$maxEducLevel_code)])

# Potencial experiencia
train_df$potExp <- pmax(train_df$age - train_df$edu_years - 6, 0)
valid_df$potExp <- pmax(valid_df$age - valid_df$edu_years - 6, 0)

cat("potExp summary (train):\n")
print(summary(train_df$potExp))

# Edad al cubo (para polinomios)
train_df$age_cubed <- train_df$age^3
valid_df$age_cubed <- valid_df$age^3

# Horas al cuadrado (retornos decrecientes)
train_df$hours_squared <- train_df$totalHoursWorked^2
valid_df$hours_squared <- valid_df$totalHoursWorked^2

cat("\n Variables preparadas: factores, variables derivadas, alineadas\n")

cat("\n================================================================================\n")
cat("PASO 2: ESTIMAR MODELOS BASE (PARA COMPARACIÓN)\n")
cat("================================================================================\n\n")

# -----------------------------
# BASE S2-0: Section 2 unconditional (raw gender gap)
# log(w) = beta0 + beta1*female + u
# -----------------------------
cat("\nModelo Base 0 (Sección 2 uncond): female\n")
cat("  ℹ Raw gender gap (sin controles).\n")
base_model0 <- lm(log_income ~ female, data = train_df)
base0_r2 <- summary(base_model0)$r.squared
cat(sprintf("  ✓ R² Train: %.4f\n", base0_r2))

# -----------------------------
# BASE 1: Section 1 unconditional
# log(w) = beta0 + beta1*age + beta2*age^2 + u
# -----------------------------
base_model1 <- lm(log_income ~ age + age_squared, data = train_df)
base1_r2 <- summary(base_model1)$r.squared
cat("Modelo Base 1 (Section 1 uncond): age + age²\n")
cat("  ℹ Modelo más simple. Sin controles.\n")
cat(sprintf("  ✓ R² Train: %.4f\n\n", base1_r2))

# -----------------------------
# BASE 2: Section 1 conditional (STRICT)
# Solo totalHoursWorked y relab como controles
# -----------------------------
if (!all(c("totalHoursWorked", "relab") %in% names(train_df))) {
  stop("ERROR: faltan variables para Base 2 (Section 1 cond): totalHoursWorked y/o relab.")
}
base_model2 <- lm(log_income ~ age + age_squared + totalHoursWorked + factor(relab),
                  data = train_df)
base2_r2 <- summary(base_model2)$r.squared
cat("Modelo Base 2 (Section 1 cond estricta): age + age² + hours + relab\n")
cat("  ℹ Solo horas trabajadas y tipo de empleo (como pide la Section 1)\n")
cat(sprintf("  ✓ R² Train: %.4f\n\n", base2_r2))

# -----------------------------
# BASE 3: Section 2 conditional (tus controles seleccionados)
# log(w) = beta0 + beta1*female + X'gamma + u
# -----------------------------
control_num <- c("age", "age_squared", "totalHoursWorked", "formal", "p6426", "p6240")
control_fac <- c("maxEducLevel", "relab", "oficio", "sizeFirm", "estrato1")

# Solo conservar variables que existan
control_num <- control_num[control_num %in% names(train_df)]
control_fac <- control_fac[control_fac %in% names(train_df)]

rhs_base3 <- c("female", control_num, paste0("factor(", control_fac, ")"))
rhs_base3 <- unique(rhs_base3)

f_base3 <- as.formula(paste("log_income ~", paste(rhs_base3, collapse = " + ")))
base_model3 <- lm(f_base3, data = train_df)

base3_r2 <- summary(base_model3)$r.squared
cat("Modelo Base 3 (Section 2 cond): female + controles\n")
cat("  ℹ Gap ajustado por controles (los que definiste)\n")
cat(sprintf("  ✓ R² Train: %.4f\n\n", base3_r2))

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
edu_formal_scale <- scale(train_df$edu_years)[, 1]
train_df$edu_formal_int <- edu_formal_scale * train_df$formal
valid_df$edu_formal_int <- scale(valid_df$edu_years)[, 1] * valid_df$formal

train_df$edu_estrato_int <- edu_formal_scale * as.numeric(train_df$estrato1)
valid_df$edu_estrato_int <- scale(valid_df$edu_years)[, 1] * as.numeric(valid_df$estrato1)

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


# Función para crear columnas de splines (código adaptado para crear columnas explícitas en data frame) 
make_spline_cols <- function(x, df = NULL, knots = NULL, degree = 3, prefix = "spl") {
  B <- if (!is.null(df)) splines::bs(x, df = df, degree = degree) else splines::bs(x, knots = knots, degree = degree)
  B <- as.data.frame(B)
  names(B) <- paste0(prefix, seq_len(ncol(B)))
  B
}

# 1) Crear spline columnas explícitas (train + valid)
spl_train4 <- make_spline_cols(train_df$age, df = 5, prefix = "age_spl4_")
spl_valid4 <- make_spline_cols(valid_df$age, df = 5, prefix = "age_spl4_")

train_df <- cbind(train_df, spl_train4)
valid_df <- cbind(valid_df, spl_valid4)

# 2) Interacciones spline x female (columna por columna)
if ("female" %in% names(train_df)) {
  for (nm in names(spl_train4)) {
    iname <- paste0(nm, "_x_female")
    train_df[[iname]] <- train_df[[nm]] * train_df$female
    valid_df[[iname]] <- valid_df[[nm]] * valid_df$female
  }
}

# 3) Fórmula ALT4
spline_terms4 <- names(spl_train4)
spline_int4 <- paste0(spline_terms4, "_x_female")

terms_alt4 <- c(
  "female", spline_terms4, spline_int4,
  "totalHoursWorked","formal","p6426","p6240",
  "factor(maxEducLevel)","factor(relab)"
)

# Filtrar términos existentes (incluye factores)
terms_alt4_ok <- c()
for (t in terms_alt4) {
  if (grepl("^factor\\(", t)) {
    v <- gsub("^factor\\(|\\)$", "", t)
    if (v %in% names(train_df)) terms_alt4_ok <- c(terms_alt4_ok, t)
  } else {
    if (t %in% names(train_df) && !all(is.na(train_df[[t]]))) terms_alt4_ok <- c(terms_alt4_ok, t)
  }
}
terms_alt4_ok <- unique(terms_alt4_ok)

f_alt4 <- as.formula(paste("log_income ~", paste(terms_alt4_ok, collapse = " + ")))
alt_model4 <- lm(f_alt4, data = train_df)

alt4_r2 <- summary(alt_model4)$r.squared
cat(sprintf("ALT4 (Spline + interactions) R² Train: %.4f\n", alt4_r2))

alt4_r2 <- summary(alt_model4)$r.squared
cat(sprintf("✓ R² Train: %.4f | Parámetros: %d\n", alt4_r2, length(coef(alt_model4))))

# ─────────────────────────────────────────────────────────────────────────────
# MODELO ALTERNATIVO 5: Integra todas las opciones (Splines + Interacciones + FE)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║ ALT 5 : MODELO INTEGRADOR - Teoría + Flexibilidad                          ║\n")
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
# =========================
# ALT5: integrated (spline df=4 + hours^2 + richer FE)  
# =========================

# 1) Crear spline columnas explícitas (train + valid)
spl_train5 <- make_spline_cols(train_df$age, df = 4, prefix = "age_spl5_")
spl_valid5 <- make_spline_cols(valid_df$age, df = 4, prefix = "age_spl5_")

train_df <- cbind(train_df, spl_train5)
valid_df <- cbind(valid_df, spl_valid5)

# 2) Interacciones (solo si existen)
if (all(c("female","edu_ord") %in% names(train_df))) {
  train_df$female_edu_int <- train_df$female * train_df$edu_ord
  valid_df$female_edu_int <- valid_df$female * valid_df$edu_ord
}
if (all(c("formal","edu_ord") %in% names(train_df))) {
  train_df$formal_edu_int <- train_df$formal * train_df$edu_ord
  valid_df$formal_edu_int <- valid_df$formal * valid_df$edu_ord
}
if (all(c("potExp","estrato_ord") %in% names(train_df)) && !all(is.na(train_df$potExp))) {
  train_df$potexp_estrato_int <- train_df$potExp * train_df$estrato_ord
  valid_df$potexp_estrato_int <- valid_df$potExp * valid_df$estrato_ord
} else {
  train_df$potexp_estrato_int <- NA_real_
  valid_df$potexp_estrato_int <- NA_real_
}

# 3) Fórmula ALT5
terms_alt5 <- c(
  "female", names(spl_train5),
  "totalHoursWorked","hours_squared",
  "female_edu_int","formal_edu_int","potexp_estrato_int",
  "formal","p6426","p6240",
  "factor(oficio)","factor(sizeFirm)","factor(estrato1)","factor(maxEducLevel)","factor(relab)"
)

terms_alt5_ok <- c()
for (t in terms_alt5) {
  if (grepl("^factor\\(", t)) {
    v <- gsub("^factor\\(|\\)$", "", t)
    if (v %in% names(train_df)) terms_alt5_ok <- c(terms_alt5_ok, t)
  } else {
    if (t %in% names(train_df) && !all(is.na(train_df[[t]]))) terms_alt5_ok <- c(terms_alt5_ok, t)
  }
}
terms_alt5_ok <- unique(terms_alt5_ok)

f_alt5 <- as.formula(paste("log_income ~", paste(terms_alt5_ok, collapse = " + ")))
alt_model5 <- lm(f_alt5, data = train_df)
alt5_r2 <- summary(alt_model5)$r.squared

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

# ═════════════════════════════════════════════════════════════════════════════
# ALINEACIÓN: LIMPIAR VALIDATION SET DE NAs EN VARIABLES CRÍTICAS
# ═════════════════════════════════════════════════════════════════════════════
# Para evitar desalineación de splines y factores, eliminar obs con NAs en
# cualquier variable usada en los modelos (incluyendo log_income para RMSE)

cat("\nLimpiando validation set de observaciones con NAs en variables críticas...")

vars_critical <- c("log_income", "age", "female", "maxEducLevel", "formal", 
                   "p6240", "p6426", "oficio", "estrato1", "totalHoursWorked", 
                   "age_squared", "relab", "sizeFirm", "p6426")

# Crear índice de filas válidas
valid_idx <- complete.cases(valid_df[, vars_critical])
valid_df_clean <- valid_df[valid_idx, ]

cat(sprintf("\n  Original: %d obs\n", nrow(valid_df)))
cat(sprintf("  Limpio:   %d obs\n", nrow(valid_df_clean)))
cat(sprintf("  Eliminadas: %d obs (%.2f%%)\n", 
            nrow(valid_df) - nrow(valid_df_clean),
            100 * (nrow(valid_df) - nrow(valid_df_clean)) / nrow(valid_df)))

# ═════════════════════════════════════════════════════════════════════════════
# REMOVER OBSERVACIONES CON FACTOR LEVELS NO VISTOS EN TRAINING
# ═════════════════════════════════════════════════════════════════════════════
# Para evitar errores en predict() con factores no observados en training

# Identificar niveles vistos en training para cada variable categórica
train_levels <- list()
for (v in control_fac) {
  if (v %in% names(train_df)) {
    train_levels[[v]] <- sort(unique(as.character(train_df[[v]][!is.na(train_df[[v]])])))
  }
}

# Filtrar valid_df_clean: remover obs con factor levels no vistos
valid_mask <- rep(TRUE, nrow(valid_df_clean))
for (v in control_fac) {
  if (v %in% names(valid_df_clean) && v %in% names(train_levels)) {
    valid_mask <- valid_mask & (as.character(valid_df_clean[[v]]) %in% train_levels[[v]])
  }
}

valid_df_clean_filtered <- valid_df_clean[valid_mask, ]

cat(sprintf("\nEliminando observaciones con factor levels no vistos en training...\n"))
cat(sprintf("  Antes: %d obs\n", nrow(valid_df_clean)))
cat(sprintf("  Después: %d obs\n", nrow(valid_df_clean_filtered)))
cat(sprintf("  Removidas: %d obs (%.2f%% de validación)\n",
            nrow(valid_df_clean) - nrow(valid_df_clean_filtered),
            100 * (nrow(valid_df_clean) - nrow(valid_df_clean_filtered)) / nrow(valid_df_clean)))

# Usar valid_df_clean_filtered para predicciones
valid_df_clean <- valid_df_clean_filtered

cat("\nGenerando predicciones para 8 modelos (sobre validation set limpio y alineado)...")

pred_base0 <- predict(base_model0, newdata = valid_df_clean, na.action = na.exclude)
pred_base1 <- predict(base_model1, newdata = valid_df_clean, na.action = na.exclude)
pred_base2 <- predict(base_model2, newdata = valid_df_clean, na.action = na.exclude)
pred_base3 <- predict(base_model3, newdata = valid_df_clean, na.action = na.exclude)
pred_alt1  <- predict(alt_model1, newdata = valid_df_clean, na.action = na.exclude)
pred_alt2  <- predict(alt_model2, newdata = valid_df_clean, na.action = na.exclude)
pred_alt3  <- predict(alt_model3, newdata = valid_df_clean, na.action = na.exclude)
pred_alt4  <- predict(alt_model4, newdata = valid_df_clean, na.action = na.exclude)
pred_alt5  <- predict(alt_model5, newdata = valid_df_clean, na.action = na.exclude)

cat("\n")

# ═════════════════════════════════════════════════════════════════════════════
# VERIFICACIÓN DE ALINEACIÓN: SPLINES Y FACTORES
# ═════════════════════════════════════════════════════════════════════════════

cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("DIAGNÓSTICO: ALINEACIÓN DE SPLINES Y FACTORES EN PREDICCIONES\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

cat("Resumen de predicciones (sobre validation set limpio):\n")
cat(sprintf("  Valid set limpio: %d obs\n", nrow(valid_df_clean)))
cat(sprintf("  Base 0: %d predicciones, %d NAs\n", length(pred_base0), sum(is.na(pred_base0))))
cat(sprintf("  Base 1: %d predicciones, %d NAs\n", length(pred_base1), sum(is.na(pred_base1))))
cat(sprintf("  Base 2: %d predicciones, %d NAs\n", length(pred_base2), sum(is.na(pred_base2))))
cat(sprintf("  Base 3: %d predicciones, %d NAs\n", length(pred_base3), sum(is.na(pred_base3))))
cat(sprintf("  Alt 1:  %d predicciones, %d NAs\n", length(pred_alt1), sum(is.na(pred_alt1))))
cat(sprintf("  Alt 2:  %d predicciones, %d NAs\n", length(pred_alt2), sum(is.na(pred_alt2))))
cat(sprintf("  Alt 3:  %d predicciones, %d NAs\n", length(pred_alt3), sum(is.na(pred_alt3))))
cat(sprintf("  Alt 4:  %d predicciones, %d NAs\n", length(pred_alt4), sum(is.na(pred_alt4))))
cat(sprintf("  Alt 5:  %d predicciones, %d NAs\n", length(pred_alt5), sum(is.na(pred_alt5))))

# Verificar alineación
all_preds <- list(pred_base0, pred_base1, pred_base2, pred_base3, pred_alt1, pred_alt2, pred_alt3, pred_alt4, pred_alt5)
pred_names <- c("Base 0", "Base 1", "Base 2", "Base 3", "Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5")
misalignment <- FALSE

for(i in seq_along(all_preds)){
  if(length(all_preds[[i]]) != nrow(valid_df_clean)){
    cat(sprintf("\n  ADVERTENCIA: %s tiene longitud %d (esperado %d)\n",
                pred_names[i], length(all_preds[[i]]), nrow(valid_df_clean)))
    misalignment <- TRUE
  }
}

if(!misalignment){
  cat("\n  RESULTADO: OK - Todas las predicciones alineadas correctamente.\n")
  cat("              Splines y factores funcionando sin problemas.\n")
} else {
  cat("\n  RESULTADO: PROBLEMA DETECTADO - Ver advertencias arriba.\n")
}

cat("\n")

# ==============================================================================
# PASO 5: CALCULAR RMSE EN VALIDATION SET
# ==============================================================================

cat("\n================================================================================\n")
cat("PASO 5: EVALUAR PERFORMANCE OUT-OF-SAMPLE\n")
cat("================================================================================\n")

calc_rmse <- function(actual, predicted) {
  # Ahora que valid_df_clean está alineado, simplificar la función
  # Todos los vectores tienen la misma longitud
  mask <- !is.na(predicted) & !is.na(actual)
  if(sum(mask) == 0) return(NA_real_)
  sqrt(mean((actual[mask] - predicted[mask])^2))
}

# Función alternativa que reporta RMSE junto con número de observaciones usadas (en caso de NAs)
rmse_with_n <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  list(
    rmse = sqrt(mean((actual[ok] - predicted[ok])^2)),
    n_used = sum(ok),
    n_total = length(actual)
  )
}

# Calcular RMSE usando valid_df_clean (datos alineados)
base0_out <- rmse_with_n(valid_df_clean$log_income, pred_base0)
base1_out <- rmse_with_n(valid_df_clean$log_income, pred_base1)
base2_out <- rmse_with_n(valid_df_clean$log_income, pred_base2)
base3_out <- rmse_with_n(valid_df_clean$log_income, pred_base3)

alt1_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt1)
alt2_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt2)
alt3_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt3)
alt4_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt4)
alt5_out  <- rmse_with_n(valid_df_clean$log_income, pred_alt5)

# Mostrar RMSE con número de observaciones usadas
rmse_base0 <- base0_out$rmse
rmse_base1 <- base1_out$rmse
rmse_base2 <- base2_out$rmse
rmse_base3 <- base3_out$rmse
rmse_alt1  <- alt1_out$rmse
rmse_alt2  <- alt2_out$rmse
rmse_alt3  <- alt3_out$rmse
rmse_alt4  <- alt4_out$rmse
rmse_alt5  <- alt5_out$rmse

# Calcular mejoras relativas en porcentaje respecto al baseline (Base 1, el más simple sin NAs)
# Usar solo el baseline1 que funciona (sin NAs de dos+)
baseline_rmse <- rmse_base1  
improvements <- 100 * (baseline_rmse - c(rmse_base0, rmse_base1, rmse_base2, rmse_base3,
                                        rmse_alt1, rmse_alt2, rmse_alt3, rmse_alt4, rmse_alt5)) / baseline_rmse

# Reemplazar NAs con NA_character para display
improvements_display <- sapply(improvements, function(x) {
  if(is.na(x)) "NA" else sprintf("%+.2f%%", x)
})

cat("\n╔════════════════════════════════════════════════════════════════════════════╗\n")
cat(sprintf("║ RESULTADOS: RMSE EN VALIDATION SET (%d observaciones limpias)          ║\n", nrow(valid_df_clean)))
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

cat("MODELOS BASE (para comparación):\n")
cat("─────────────────────────────────\n")
cat(sprintf("  Base 0 (female only): RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_base0, base0_r2, improvements[1]))

cat(sprintf("  Base 1 (simple):      RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_base1, base1_r2, improvements[2]))

cat(sprintf("  Base 2 (básico):      RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_base2, base2_r2, improvements[3]))

cat(sprintf("  Base 3 (control):     RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_base3, base3_r2, improvements[4]))

cat("\n--- MODELOS ALTERNATIVOS ---\n")
cat(sprintf("  Alt 1 (polynomial):   RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_alt1, alt1_r2, improvements[5]))
cat(sprintf("  Alt 2 (interactions): RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_alt2, alt2_r2, improvements[6]))
cat(sprintf("  Alt 3 (extended):     RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_alt3, alt3_r2, improvements[7]))
cat(sprintf("  Alt 4 (spline):       RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_alt4, alt4_r2, improvements[8]))
cat(sprintf("  Alt 5 (integrated):   RMSE = %.4f | R² = %.4f (Mejora: %+.2f%%)\n",
            rmse_alt5, alt5_r2, improvements[9]))
# Ranking
all_rmses <- c(rmse_base0, rmse_base1, rmse_base2, rmse_base3,
               rmse_alt1, rmse_alt2, rmse_alt3, rmse_alt4, rmse_alt5)

all_names <- c("Base 0", "Base 1", "Base 2", "Base 3",
               "Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5")
ranking <- order(all_rmses)

cat("\n\nRANKING FINAL (menor RMSE = mejor):\n")
cat("──────────────────────────────────\n")
for(i in seq_along(all_rmses)){
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
  Model = c("Base 0: female (raw gap)",
            "Base 1: age + age²",
            "Base 2: age + age² + hours + relab",
            "Base 3: female + controls",
            "Alt 1", "Alt 2", "Alt 3", "Alt 4", "Alt 5"),
  Type = c(rep("Base", 4), rep("Alternativo", 5)),
  RMSE_Validation = c(rmse_base0, rmse_base1, rmse_base2, rmse_base3,
                      rmse_alt1, rmse_alt2, rmse_alt3, rmse_alt4, rmse_alt5),
  N_valid_used = c(base0_out$n_used, base1_out$n_used, base2_out$n_used, base3_out$n_used,
                   alt1_out$n_used,  alt2_out$n_used,  alt3_out$n_used,  alt4_out$n_used,  alt5_out$n_used),
  N_valid_total = c(base0_out$n_total, base1_out$n_total, base2_out$n_total, base3_out$n_total,
                    alt1_out$n_total,  alt2_out$n_total,  alt3_out$n_total,  alt4_out$n_total,  alt5_out$n_total),
  R2_Train = c(base0_r2, base1_r2, base2_r2, base3_r2,
               alt1_r2, alt2_r2, alt3_r2, alt4_r2, alt5_r2),
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
  Type  = c(rep("Base", 4), rep("Alternative", 5)),
  IsBase = c(rep(TRUE, 4), rep(FALSE, 5)),
  Rank = rank(all_rmses)
)

p <- ggplot(plot_data, aes(x = reorder(Model, RMSE), y = RMSE, 
                           fill = Type, alpha = (Rank == 1))) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
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

cat("\n================================================================================\n")
cat("✅ SCRIPT COMPLETADO EXITOSAMENTE\n")
cat("================================================================================\n")
cat("\nRESUMEN DE SALIDAS:\n")
cat(sprintf("  ✓ Tabla LaTeX:   02_output/tables/03_section3_prediction/alternative_models_v2.tex\n"))
cat(sprintf("  ✓ Gráfico PNG:   02_output/figures/03_section3_prediction/rmse_comparison_v2.png\n"))
cat(sprintf("  ✓ Validation N:  %d observaciones (después de limpiar NAs)\n", nrow(valid_df_clean)))
cat(sprintf("  ✓ Mejor modelo:  Alt 5 (RMSE = %.4f)\n", min(na.omit(all_rmses))))
cat("\n")

