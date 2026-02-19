################################################################################
# PROBLEM SET 1: Predicting Income
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
set.seed(123)

#Instalar librerías
pkgs <- c("ggplot2","stargazer","dplyr","boot","gt")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if(length(to_install) > 0) install.packages(to_install)

# Cargar librerías
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(boot)
  library(gt)
})
# Configuración de rutas

out_tab <- "02_output/tables/02_section2_gendergap_income"
out_fig <- "02_output/figures/02_section2_gendergap_income"
# Ensure output directories exist
if (!dir.exists(out_tab)) dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_fig)) dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

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
model_uncond <- lm(log_income ~ female, data = data)
s_uncond <- summary(model_uncond)

cat("\nModelo 1: log(ingreso) = β0 + β1*female + u\n")
cat(sprintf("  beta_1: %s\n", coef(model_uncond)[["female"]]))
cat(sprintf("  Observaciones: %s\n", format(nobs(model_uncond), big.mark=",")))
cat(sprintf("  R²: %.4f\n", s_uncond$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", s_uncond$adj.r.squared))

cat("\n================================================================================\n") # nolint: line_length_linter.
cat("PASO 3: REGRESIÓN FWL\n")
cat("================================================================================\n") # nolint

# Función FWL
run_fwl_gender <- function(df, control_list,
                          y = "log_income",
                          g = "female") {
  
   results <- vector("list", length(control_list))
  
  for (i in seq_along(control_list)) {
    controls_str <- paste(control_list[[i]], collapse = " + ")
    f <- as.formula(paste(y, "~", g, "+", controls_str))

    model <- lm(f, data = df)
    summ <- summary(model)
    
    results[[i]] <- data.frame(
      model_id = paste0("M", i),
      controls = paste(control_list[[i]], collapse = ", "),
      n = nobs(model),
      beta_female = coef(model)[g],
      se_female = summ$coefficients[g, "Std. Error"],
      p_value = summ$coefficients[g, "Pr(>|t|)"],
      adj_r2 = summ$adj.r.squared,
      aic = AIC(model)
    )
  }
  do.call(rbind, results)
}

#lista de Controles a comparar
control_sets <- list(
c("age", "age_squared", "factor(maxEducLevel)"), #Baseline M1
c("age", "age_squared", "factor(maxEducLevel)", "totalHoursWorked"), #Intensidad laboral M2
c("age", "age_squared", "factor(maxEducLevel)", "totalHoursWorked", "factor(relab)","factor(oficio)"), #Estructura oupacional M3
c("age", "age_squared", "factor(maxEducLevel)", "totalHoursWorked", "factor(relab)","factor(oficio)",
"factor(sizeFirm)","formal", "p6426"),
c("age", "age_squared", "factor(maxEducLevel)", "totalHoursWorked", "factor(relab)","factor(oficio)",
"factor(sizeFirm)","formal", "p6426", "factor(p6240)","factor(estrato1)") #Empresa y condiciones de vida M5
)

#Ejecutar modelo
model_comparison <- run_fwl_gender(data, control_sets)

tab <- model_comparison
tab$rank_adj_r2 <- rank(-tab$adj_r2, ties.method = "min")
tab$rank_aic    <- rank(tab$aic, ties.method = "min")
tab$rank_total  <- tab$rank_adj_r2 + tab$rank_aic
tab <- tab[order(tab$rank_total), ]

tab$beta_female <- round(tab$beta_female, 4)
tab$se_female   <- round(tab$se_female, 4)
tab$adj_r2      <- round(tab$adj_r2, 4)
tab$aic         <- round(tab$aic, 2)
tab$p_value     <- signif(tab$p_value, 3)

print(tab[, c("model_id","n","beta_female","se_female","p_value","adj_r2","aic",
              "rank_adj_r2","rank_aic","rank_total","controls")],
      row.names = FALSE)

tab_export <- tab[, c("model_id","beta_female","se_female", "adj_r2","aic")]

tabla_png <- tab_export |>
  gt() |>
  tab_header(
    title = "Comparación modelos",
    subtitle = "Ordenados por ranking combinado (Adj. R² + AIC)"
  ) |>
  cols_label(
    model_id = "Modelo",
    beta_female = "Beta (female)",
    se_female = "SE",
    adj_r2 = "Adj. R²",
    aic = "AIC",
  ) |>
  fmt_number(
    columns = c(beta_female, se_female, adj_r2),
    decimals = 4
  ) |>
  fmt_number(
    columns = aic,
    decimals = 2
  )

gtsave(tabla_png, filename = "model_comparison.png", path = out_tab)

# Best model 
best_id <- tab$model_id[1]
best_idx <- as.integer(sub("M", "", best_id))
best_controls <- control_sets[[best_idx]]
best_controls_str <- paste(best_controls, collapse = " + ")
f_best_ols <- as.formula(paste("log_income ~ female +", best_controls_str))

cat(sprintf("\nUsando el mejor modelo según ranking: %s\n", best_id))
cat("Controles:\n")
cat(paste0("  ", paste(best_controls, collapse = " + "), "\n"))

cat("\n================================================================================\n") # nolint
cat("PASO 4: REGRESIÓN CONDICIONAL SIN FWL\n")
cat("================================================================================\n") # nolint

# Gap condicional con controles para verificación modelo FWL

model_cond_ols <- lm(f_best_ols, data = data)
s_cond_ols <- summary(model_cond_ols)

beta_ols <- coef(model_cond_ols)[["female"]]
se_ols   <- s_cond_ols$coefficients["female", "Std. Error"]
t_ols    <- s_cond_ols$coefficients["female", "t value"]
p_ols    <- s_cond_ols$coefficients["female", "Pr(>|t|)"]

cat("\nModelo OLS condicional (sin FWL)\n")
cat(sprintf("  beta_female: %.4f\n", beta_ols))
cat(sprintf("  SE: %.4f\n", se_ols))
cat(sprintf("  t: %.3f\n", t_ols))
cat(sprintf("  p-value: %.4g\n", p_ols))
cat(sprintf("  N: %s\n", format(nobs(model_cond_ols), big.mark=",")))
cat(sprintf("  R²: %.4f\n", s_cond_ols$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", s_cond_ols$adj.r.squared))
cat(sprintf("  AIC: %.2f\n", AIC(model_cond_ols)))

## FWL una sola vez (misma especificación)
run_fwl_once <- function(df, controls, y = "log_income", g = "female") {
  controls_str <- paste(controls, collapse = " + ")
  f_g <- as.formula(paste(g, "~", controls_str))
  f_y <- as.formula(paste(y, "~", controls_str))

  g_res <- resid(lm(f_g, data = df))
  y_res <- resid(lm(f_y, data = df))

  fwl <- lm(y_res ~ g_res)
  s <- summary(fwl)
    list(
    beta = coef(fwl)[["g_res"]],
    se   = s$coefficients["g_res", "Std. Error"],
    t    = s$coefficients["g_res", "t value"],
    p    = s$coefficients["g_res", "Pr(>|t|)"],
    model = fwl
  )
}

fwl_out <- run_fwl_once(data, best_controls)
beta_hat <- fwl_out$beta
se_fwl   <- fwl_out$se
t_fwl    <- fwl_out$t
p_fwl    <- fwl_out$p

cat("\nFWL (residualización) para el mismo set de controles\n")
cat(sprintf("  beta_female (FWL): %.4f\n", beta_hat))
cat(sprintf("  SE (FWL): %.4f\n", se_fwl))
cat(sprintf("  t (FWL): %.3f\n", t_fwl))
cat(sprintf("  p-value (FWL): %.4g\n", p_fwl))


cat("\nCheck FWL vs OLS condicional:\n")
cat(sprintf("  beta_female (OLS): %.6f\n", beta_ols))
cat(sprintf("  beta_female (FWL): %.6f\n", beta_hat))
cat(sprintf("  Diferencia (debe ser ~0): %.10f\n", beta_ols - beta_hat))

#  Mini-tabla de comparación
comparison<- data.frame(
  metodo = c("OLS condicional", "FWL"),
  beta_female = c(beta_ols, beta_hat),
  se = c(se_ols, se_fwl),
  t = c(t_ols, t_fwl),
  p_value = c(p_ols, p_fwl))

comparison$beta_female <- round(comparison$beta_female, 4)
comparison$se <- round(comparison$se, 4)
comparison$t <- round(comparison$t, 3)
comparison$p_value <- signif(comparison$p_value, 3)

cat("\nResumen comparación (solo coef female):\n")
print(comparison, row.names = FALSE)

cat("\n================================================================================\n") # nolint
cat("PASO 5: BOOTSTRAP\n")
cat("================================================================================\n") # nolint

run_fwl_bootstrap <- function(df, controls, R = 500, y = "log_income", g = "female") {
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

boot_res <- run_fwl_bootstrap(data, controls = best_controls, R = 500)
se_boot <- sd(boot_res$t)
ci_boot <- boot::boot.ci(boot_res, type = c("perc", "basic"))

# SE analítico condicional
se_anal <- s_cond_ols$coefficients["female", "Std. Error"]

cat("\n--- Resultados Gender Gap (Condicional) ---\n")
cat(sprintf("N: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("beta_female (FWL): %.4f\n", beta_hat))
cat(sprintf("SE analítico (OLS): %.4f\n", se_anal))
cat(sprintf("SE bootstrap: %.4f\n", se_boot))
cat("\nIC bootstrap (ver objeto ci_boot):\n")
print(ci_boot)

cat("\n================================================================================\n") # nolint
cat("PASO 6: TABLA COMPARATIVA CONDICIONAL E INCONDICIONAL\n")
cat("================================================================================\n") # nolint

# Uncondicional: 
beta_uncond <- coef(model_uncond)[["female"]]
se_uncond_anal <- s_uncond$coefficients["female", "Std. Error"]
r2_uncond <- s_uncond$adj.r.squared
n_uncond <- nobs(model_uncond)

# Condicional:
beta_cond <- coef(model_cond_ols)[["female"]]
se_cond_anal <- s_cond_ols$coefficients["female", "Std. Error"]
r2_cond <- s_cond_ols$adj.r.squared
n_cond <- nobs(model_cond_ols)

# Bootstrap para ambos modelos
boot_uncond_fun <- function(d, idx){
  dd <- d[idx, ]
  coef(lm(log_income ~ female, data = dd))[["female"]]
}
boot_uncond <- boot::boot(data, statistic = boot_uncond_fun, R = 500)
se_uncond_boot <- sd(boot_uncond$t)

boot_cond_fun <- function(d, idx){
  dd <- d[idx, ]
  coef(lm(f_best_ols, data = dd))[["female"]]
}
boot_cond <- boot::boot(data, statistic = boot_cond_fun, R = 500)
se_cond_boot <- sd(boot_cond$t)

results_table <- data.frame(
  Specification = c("Unconditional", paste0("Conditional (", best_id, ")")),
  Beta_Female = c(beta_uncond, beta_cond),
  SE_Analytical = c(se_uncond_anal, se_cond_anal),
  SE_Bootstrap = c(se_uncond_boot, se_cond_boot),
  Adj_R_squared = c(r2_uncond, r2_cond)
)

results_table$Beta_Female <- round(results_table$Beta_Female, 4)
results_table$SE_Analytical <- round(results_table$SE_Analytical, 4)
results_table$SE_Bootstrap <- round(results_table$SE_Bootstrap, 4)
results_table$Adj_R_squared <- round(results_table$Adj_R_squared, 4)


print(results_table, row.names = FALSE)

gt_tbl <- gt(results_table) |>
  tab_header(
    title = "Gender Labor Income Gap",
    subtitle = "Unconditional vs Conditional (Analytical SE vs Bootstrap SE)"
  ) |>
  cols_label(
    Specification = "Specification",
    Beta_Female = "Beta (female)",
    SE_Analytical = "SE (Analytical)",
    SE_Bootstrap = "SE (Bootstrap)",
    Adj_R_squared = "Adj_R²"
  )

gtsave(gt_tbl, filename = "02_gender_gap_table.png", path = out_tab)


cat("\n================================================================================\n")
cat("PASO 7: VISUALIZACIÓN PREDICTED AGE-LABOR INCOME PROFILES\n")
cat("================================================================================\n")

model_interact <- lm(  
  log_income ~ female*(age + age_squared) +totalHoursWorked +factor(maxEducLevel) + factor(relab) + factor(oficio) + factor(sizeFirm) + formal + p6426 + factor(p6240) + factor(estrato1), data = data) # nolint

cat("\nModelo interacción estimado:\n")
print(summary(model_interact)$coefficients[c("female","age","age_squared","female:age","female:age_squared"), , drop=FALSE]) # nolint

age_seq <- seq(min(data$age, na.rm = TRUE),
               max(data$age, na.rm = TRUE),
               by = 1)

mean_hours <- mean(data$totalHoursWorked, na.rm = TRUE)
base_relab <- levels(factor(data$relab))[1]
base_educ  <- levels(factor(data$maxEducLevel))[1]
base_oficio <- levels(factor(data$oficio))[1]
base_sizeFirm <- levels(factor(data$sizeFirm))[1]
base_estrato1 <- levels(factor(data$estrato1))[1]
base_p6240 <- levels(factor(data$p6240))[1]


pred_data <- expand.grid(age = age_seq, female = c(0, 1))
pred_data$age_squared <- pred_data$age^2
pred_data$totalHoursWorked <- mean_hours
pred_data$formal <- mean(data$formal, na.rm = TRUE)
pred_data$p6426 <- mean(data$p6426, na.rm = TRUE)

pred_data$relab <- factor(base_relab, levels = levels(factor(data$relab)))
pred_data$maxEducLevel <- factor(base_educ, levels = levels(factor(data$maxEducLevel)))
pred_data$oficio <- factor(base_oficio, levels = levels(factor(data$oficio)))
pred_data$sizeFirm <- factor(base_sizeFirm, levels = levels(factor(data$sizeFirm)))
pred_data$p6240 <- factor(base_p6240, levels = levels(factor(data$p6240)))
pred_data$estrato1 <- factor(base_estrato1, levels = levels(factor(data$estrato1)))

pred_data$pred_log <- as.numeric(predict(model_interact, newdata = pred_data))
pred_data$pred_income <- exp(pred_data$pred_log)

pred_data$gender <- factor(pred_data$female,
                           levels = c(0, 1),
                           labels = c("Male", "Female"))

peaks <- pred_data %>%
  group_by(gender) %>%
  slice_max(order_by = pred_income, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label_y = pred_income * 1.05)

cat("\nPeaks (por grid de edad):\n")
print(peaks %>% select(gender, age, pred_income))

cfs <- coef(model_interact)
b_age   <- cfs[["age"]]
b_age2  <- cfs[["age_squared"]]
b_fage  <- if ("female:age" %in% names(cfs)) cfs[["female:age"]] else 0
b_fage2 <- if ("female:age_squared" %in% names(cfs)) cfs[["female:age_squared"]] else 0

peak_age_male_analytic <- -b_age / (2*b_age2)
peak_age_fem_analytic  <- -(b_age + b_fage) / (2*(b_age2 + b_fage2))

cat("\nPeaks analíticos (vértice):\n")
cat(sprintf("  Peak age (Male):   %.2f\n", peak_age_male_analytic))
cat(sprintf("  Peak age (Female): %.2f\n", peak_age_fem_analytic))

peak_m <- peaks %>% filter(gender == "Male")   %>% slice(1)
peak_f <- peaks %>% filter(gender == "Female") %>% slice(1)

gap_peak_log <- peak_f$pred_log - peak_m$pred_log
gap_peak_pct <- 100*(exp(gap_peak_log) - 1)

cat("\nGap en el peak (Female vs Male, usando predicción en el peak):\n")
cat(sprintf("  Gap log: %.4f\n", gap_peak_log))
cat(sprintf("  Gap %%:  %.2f%%\n", gap_peak_pct))

p <- ggplot(pred_data, aes(age, pred_income, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = peaks, size = 3) +
  geom_text(
    data = peaks,
    aes(x = age, y = label_y,
        label = paste0("Peak age: ", age,
                       "\nIncome: ", format(round(pred_income, 0), big.mark=","))),
    show.legend = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = "Age", y = "Predicted labor income (levels)", color = "") +
  theme_bw()

print(p)

ggsave(file.path(out_fig, "age_labor_income_profiles.png"),
       plot = p, width = 10, height = 6, dpi = 300)

cat(sprintf("\nGuardado: %s\n", file.path(out_fig, "Age_labor_income_profiles.png")))


cat("\n================================================================================\n")
cat("PASO 8: IMPLIED PEAK AGES \n")
cat("================================================================================\n")

coefs <- coef(model_interact)

b_age    <- coefs[["age"]]
b_age2   <- coefs[["age_squared"]]
b_age_f  <- if ("female:age" %in% names(coefs)) coefs[["female:age"]] else 0
b_age2_f <- if ("female:age_squared" %in% names(coefs)) coefs[["female:age_squared"]] else 0

peak_male   <- -b_age / (2 * b_age2)
peak_female <- -(b_age + b_age_f) / (2 * (b_age2 + b_age2_f))

cat(sprintf("\nPeak age (Male):   %.3f\n", peak_male))
cat(sprintf("Peak age (Female): %.3f\n", peak_female))

boot_peak_fun <- function(d, idx){
  dd <- d[idx, ]
  m <- lm(log_income ~ female*(age + age_squared) +
            totalHoursWorked +
            factor(relab) +
            factor(maxEducLevel),
          data = dd)

  cfs <- coef(m)

  b_age    <- cfs[["age"]]
  b_age2   <- cfs[["age_squared"]]
  b_age_f  <- if ("female:age" %in% names(cfs)) cfs[["female:age"]] else 0
  b_age2_f <- if ("female:age_squared" %in% names(cfs)) cfs[["female:age_squared"]] else 0

  denom_m <- 2 * b_age2
  denom_f <- 2 * (b_age2 + b_age2_f)

  if (is.na(denom_m) || abs(denom_m) < 1e-10) return(c(NA_real_, NA_real_))
  if (is.na(denom_f) || abs(denom_f) < 1e-10) return(c(NA_real_, NA_real_))

  peak_m <- -b_age / denom_m
  peak_f <- -(b_age + b_age_f) / denom_f

  c(peak_m, peak_f)
}

boot_peaks <- boot::boot(data, statistic = boot_peak_fun, R = 500)

se_peak_male   <- sd(boot_peaks$t[, 1], na.rm = TRUE)
se_peak_female <- sd(boot_peaks$t[, 2], na.rm = TRUE)

ci_male   <- boot::boot.ci(boot_peaks, index = 1, type = "perc")
ci_female <- boot::boot.ci(boot_peaks, index = 2, type = "perc")

peak_table <- data.frame(
  Group = c("Male", "Female"),
  Peak_Age = c(as.numeric(peak_male), as.numeric(peak_female)),
  SE_Bootstrap = c(se_peak_male, se_peak_female),
  CI_Lower = c(ci_male$percent[4], ci_female$percent[4]),
  CI_Upper = c(ci_male$percent[5], ci_female$percent[5])
)
peak_table$Peak_Age <- round(peak_table$Peak_Age, 2)
peak_table$SE_Bootstrap <- round(peak_table$SE_Bootstrap, 2)
peak_table$CI_Lower <- round(peak_table$CI_Lower, 2)
peak_table$CI_Upper <- round(peak_table$CI_Upper, 2)

print(peak_table, row.names = FALSE)

stargazer::stargazer(
  peak_table,
  summary = FALSE,
  digits = 2,
  rownames = FALSE,
  title = "Implied Peak Ages by Gender",
  out = file.path(out_tab, "05_peak_ages_section2.tex")
)

cat(sprintf("\nGuardado TEX: %s\n", file.path(out_tab, "05_peak_ages_section2.tex")))

ggsave(file.path(out_fig, "05_age_income_profiles_with_peaks.png"),
       plot = p, width = 10, height = 6, dpi = 300)

cat(sprintf("Guardado FIG: %s\n", file.path(out_fig, "05_age_income_profiles_with_peaks.png")))