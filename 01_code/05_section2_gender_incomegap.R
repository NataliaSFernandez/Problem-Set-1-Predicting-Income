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
cat(sprintf("  Observaciones: %s\n", format(nobs(model1), big.mark=",")))
cat(sprintf("  R²: %.4f\n", summary(model1)$r.squared))
cat(sprintf("  R² ajustado: %.4f\n", summary(model1)$adj.r.squared))

cat("\n================================================================================\n")
cat("PASO 3: REGRESIÓN CONDICIONAL\n")
cat("================================================================================\n") # nolint: line_length_linter.

# Gap condicional con controles 
model2<- lm(log_income ~ female +
              age + age_squared +
              totalHoursWorked +
              factor(relab) +
              factor(maxEducLevel),
              data = data)

cat("\nModelo 2: log(ingreso) = β0 + β1*age + β2*age²\n") #MODIFICARR
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

cat("\nModelo 3 FWL: log(ingreso) = β0 + β1*age + β2*age²\n") #MODIFICARR
cat(sprintf("  Beta1: %s\n",beta_hat))
cat(sprintf("  SE %.4f\n", se_anal))


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

cat("\n--- Resultados Gender Gap (Condicional con FWL) ---\n") #REVISARRR
cat(sprintf("beta_female: %.4f\n", beta_hat))
cat(sprintf("SE analítico (OLS): %.4f\n", se_anal))
cat(sprintf("SE bootstrap (sd betas): %.4f\n", se_boot))
print(ci_boot)

cat("\n================================================================================\n")
cat("PASO 7: TABLA COMPARATIVA\n")
cat("================================================================================\n")

# Extraer coeficientes y SE
# ===============================

# Incondicional
beta_uncond <- coef(model1)["female"]
se_uncond_anal <- summary(model1)$coefficients["female","Std. Error"]
r2_uncond <- summary(model1)$r.squared

# Bootstrap incondicional
boot_uncond_fun <- function(d, idx){
  dd <- d[idx,]
  coef(lm(log_income ~ female, data = dd))["female"]
}

boot_uncond <- boot::boot(data, boot_uncond_fun, R = 500)
se_uncond_boot <- sd(boot_uncond$t)

# Condicional (modelo completo, no FWL)
beta_cond <- coef(model2)["female"]
se_cond_anal <- summary(model2)$coefficients["female","Std. Error"]
r2_cond <- summary(model2)$r.squared

# Bootstrap condicional (más simple que FWL)
boot_cond_fun <- function(d, idx){
  dd <- d[idx,]
  coef(lm(log_income ~ female +
          age + age_squared +
          totalHoursWorked +
          factor(relab) +
          factor(maxEducLevel),
          data = dd))["female"]
}

boot_cond <- boot::boot(data, boot_cond_fun, R = 500)
se_cond_boot <- sd(boot_cond$t)

results_table <- data.frame(
  Specification = c("Unconditional", "Conditional"),
  Beta_Female = c(beta_uncond, beta_cond),
  SE_Analytical = c(se_uncond_anal, se_cond_anal),
  SE_Bootstrap = c(se_uncond_boot, se_cond_boot),
  R_squared = c(r2_uncond, r2_cond)
)

print(results_table)


stargazer(results_table,
          summary = FALSE,
          digits = 4,
          rownames = FALSE,
          title = "Gender Labor Income Gap",
          out = "02_output/tables/05_gender_gap_table.tex")

cat("\nGuardado: 02_output/tables/05_gender_gap_table.tex\n")

cat("\n================================================================================\n") # nolint
cat("PASO 8: VISUALIZACIÓN PREDICTED AGE-LABOR INCOME PROFILES\n")
cat("================================================================================\n") # nolint
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
base_relab <- levels(factor(data$relab))[1]
base_educ  <- levels(factor(data$maxEducLevel))[1]

# Crear grid
pred_data <- expand.grid(
  age = age_seq,
  female = c(0, 1)
)

pred_data$age_squared <- pred_data$age^2
pred_data$totalHoursWorked <- mean_hours

# Asignar relab y educ como FACTORES con los mismos niveles del modelo
pred_data$relab <- factor(base_relab, levels = levels(factor(data$relab)))
pred_data$maxEducLevel <- factor(base_educ, levels = levels(factor(data$maxEducLevel)))

# Predicción en log
pred_data$pred_log <- as.numeric(predict(model_interact, newdata = pred_data))

# Pasar a nivel (si log_income = log(y))
pred_data$pred_income <- exp(pred_data$pred_log)

# Labels Male/Female
pred_data$gender <- factor(pred_data$female,
                           levels = c(0, 1),
                           labels = c("Male", "Female"))

# Máximos por grupo
peaks <- pred_data %>%
  group_by(gender) %>%
  slice_max(order_by = pred_income, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot
p <- ggplot(pred_data, aes(x = age, y = pred_income, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = peaks, aes(x = age, y = pred_income), size = 3) +
  geom_text(
    data = peaks,
    aes(label = paste0("Peak: age ", age, "\n", format(round(pred_income, 0), big.mark=","))),
    vjust = -1, hjust = 0.5, show.legend = FALSE
  ) +
  labs(
    title = "Predicted Age–Labor Income Profiles",
    x = "Age",
    y = "Predicted Monthly Labor Income (COP)",
    color = ""
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

print(p)

# Guardar
dir.create("02_output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("02_output/figures/05_age_labor_income_profiles.png",
       plot = p, width = 10, height = 6, dpi = 300)

cat("\nGuardado: 02_output/figures/05_age_labor_income_profiles.png\n")

cat("\n================================================================================\n")
cat("PASO 9: IMPLIED PEAK AGES \n")
cat("================================================================================\n")

coefs <- coef(model_interact)

b_age <- coefs["age"]
b_age2 <- coefs["age_squared"]

b_age_f <- coefs["female:age"]
b_age2_f <- coefs["female:age_squared"]

peak_male <- -b_age / (2*b_age2)
peak_female <- -(b_age + b_age_f) /
               (2*(b_age2 + b_age2_f))

cat("\nPeak age (Male):", peak_male)
cat("\nPeak age (Female):", peak_female)

boot_peak_fun <- function(d, idx){
  dd <- d[idx,]

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
          out = "02_output/tables/05_peak_ages_section2.tex")

