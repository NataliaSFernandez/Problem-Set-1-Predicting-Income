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

out_tab <- "02_output/tables/02_section2_gendergap_income"
out_fig <- "02_output/figures/02_section2_gendergap_income"

cat("\n================================================================================\n") # nolint
cat("PASO 0: CARGAR DATOS LIMPIOS\n")
cat("================================================================================\n") # nolint

data <- read.csv("00_data/cleaned/data_cleaned.csv")

cat(sprintf("\nDatos cargados exitosamente\n"))
cat(sprintf("  Observaciones: %s\n", format(nrow(data), big.mark=",")))
cat(sprintf("  Variables: %d\n", ncol(data)))

cat("\n================================================================================\n")
cat("PASO 1: VISUALIZACIÓN PREDICTED AGE-LABOR INCOME PROFILES INTERACCIONES\n")
cat("================================================================================\n")

#Interacción entre edad y género female x (age + age_squared) Modelo 1
model_interact <- lm(  
  log_income ~ female*(age + age_squared) +totalHoursWorked +factor(maxEducLevel) + factor(relab) + factor(oficio) + factor(sizeFirm) + formal + p6426  + factor(estrato1), data = data) # nolint

cat("\nModelo interacción estimado:\n")
print(summary(model_interact)$coefficients[c("female","age","age_squared","female:age","female:age_squared"), , drop=FALSE]) # nolint

age_seq <- seq(min(data$age, na.rm = TRUE),
               max(data$age, na.rm = TRUE),
               by = 1)

mean_hours <- mean(data$totalHoursWorked, na.rm = TRUE)

get_mode <- function(x){
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
}

base_relab      <- get_mode(data$relab)
base_educ       <- get_mode(data$maxEducLevel)
base_oficio     <- get_mode(data$oficio)
base_sizeFirm   <- get_mode(data$sizeFirm)
base_estrato1   <- get_mode(data$estrato1)

cat("\nValores base utilizados para predicción:\n")
cat("relab (modo):        ", base_relab, "\n") #1 = Obrero/empleado empresa particular más común
cat("maxEducLevel (modo): ", base_educ, "\n") #7 = Educación terciaria (técnica, tecnológica o universitaria)
cat("oficio (modo):       ", base_oficio, "\n") #Oficio de moda 45
cat("sizeFirm (modo):     ", base_sizeFirm, "\n") #Más comun sizeFirm 5 >50**
cat("estrato1 (modo):     ", base_estrato1, "\n") #Estrato 2 (más común)

pred_data <- expand.grid(age = age_seq, female = c(0, 1))
pred_data$age_squared <- pred_data$age^2

#Base variables continuas en su media
pred_data$totalHoursWorked <- mean_hours
pred_data$formal <- mean(data$formal, na.rm = TRUE)
pred_data$p6426 <- mean(data$p6426, na.rm = TRUE)

#Levels para variables categóricas en su categoría más común (moda)
pred_data$relab <- factor(base_relab, levels = levels(factor(data$relab)))
pred_data$maxEducLevel <- factor(base_educ, levels = levels(factor(data$maxEducLevel)))
pred_data$sizeFirm <- factor(base_sizeFirm, levels = levels(factor(data$sizeFirm)))
pred_data$estrato1 <- factor(base_estrato1, levels = levels(factor(data$estrato1)))
pred_data$oficio <- factor(base_oficio, levels = levels(factor(data$oficio)))

#Predict() usa los coeficientes del modelo para generar predicciones de log_income para cada combinación de edad y género, manteniendo las otras variables constantes en sus valores base. Luego, se transforma a niveles usando exp() para obtener predicciones de ingresos laborales en niveles.
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
  labs(
    title = "Predicted Age-Labor Income Profiles (Interaction)",
    subtitle = "Conditional model: controls held at representative values",
    x = "Age",
    y = "Predicted labor income (levels)",
    color = ""
  ) +
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

ggsave(file.path(out_fig, "age_labor_interaction.png"),
       plot = p, width = 10, height = 6, dpi = 300)

cat(sprintf("\nGuardado: %s\n", file.path(out_fig, "age_labor_interaction.png")))

cat("\n================================================================================\n")
cat("PASO 2: VISUALIZACIÓN PREDICTED AGE-LABOR INCOME PROFILES SIN INTERACCIONES \n")
cat("================================================================================\n")

model_4 <- lm(
  log_income ~ female + age + age_squared +
    totalHoursWorked +
    factor(maxEducLevel) +
    factor(relab) +
    factor(oficio) +
    factor(sizeFirm) +
    formal +
    p6426 +
    factor(estrato1),
  data = data
)

print(summary(model_4))

# Predicciones para graficar modelo 4
pred_data_m4 <- expand.grid(age = age_seq, female = c(0, 1))
pred_data_m4$age_squared <- pred_data_m4$age^2
pred_data_m4$totalHoursWorked <- mean_hours
pred_data_m4$formal <- mean(data$formal, na.rm = TRUE)
pred_data_m4$p6426 <- mean(data$p6426, na.rm = TRUE)

pred_data_m4$relab <- factor(base_relab, levels = levels(factor(data$relab)))
pred_data_m4$maxEducLevel <- factor(base_educ, levels = levels(factor(data$maxEducLevel)))
pred_data_m4$oficio <- factor(base_oficio, levels = levels(factor(data$oficio)))
pred_data_m4$sizeFirm <- factor(base_sizeFirm, levels = levels(factor(data$sizeFirm)))
pred_data_m4$estrato1 <- factor(base_estrato1, levels = levels(factor(data$estrato1)))

pred_data_m4$pred_log <- as.numeric(predict(model_4, newdata = pred_data_m4))
pred_data_m4$pred_income <- exp(pred_data_m4$pred_log)
pred_data_m4$gender <- factor(pred_data_m4$female, levels = c(0,1), labels = c("Male","Female"))

# 5) Graficar
p_m4 <- ggplot(pred_data_m4, aes(age, pred_income, color = gender)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Predicted Age-Labor Income Profiles (No Interaction)",
    subtitle = "Conditional model: controls held at representative values",
    x = "Age",
    y = "Predicted labor income (levels)",
    color = ""
  ) + geom_point(data = peaks, size = 3) +
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

ggsave(file.path(out_fig, "age_labor_no_interaction.png"),
       plot = p_m4, width = 10, height = 6, dpi = 300)

cat(sprintf("\nGuardado: %s\n", file.path(out_fig, "age_labor_no_interaction.png")))


cat("\n================================================================================\n")
cat("PASO 3: COMPARACIÓN MODELO SIN Y CON INTERACCIONES \n")
cat("================================================================================\n")

# --- 1) Métricas por modelo ---
summ_m4 <- summary(model_4)
summ_m5 <- summary(model_interact)

adjr2_m4 <- summ_m4$adj.r.squared
adjr2_m5 <- summ_m5$adj.r.squared

aic_m4 <- AIC(model_4); bic_m4 <- BIC(model_4)
aic_m5 <- AIC(model_interact); bic_m5 <- BIC(model_interact)

rss_m4 <- sum(residuals(model_4)^2)
rss_m5 <- sum(residuals(model_interact)^2)

# Proxy de "varianza"/complejidad: número de parámetros estimados
k_m4 <- length(coef(model_4))
k_m5 <- length(coef(model_interact))

# (Opcional) otro proxy de varianza: promedio de SE de coeficientes
mean_se_m4 <- mean(summ_m4$coefficients[, "Std. Error"], na.rm = TRUE)
mean_se_m5 <- mean(summ_m5$coefficients[, "Std. Error"], na.rm = TRUE)

# --- 2) F-test (ANOVA) entre modelos anidados ---
anova_res <- anova(model_4, model_interact)

F_stat <- anova_res$F[2]
p_val  <- anova_res$`Pr(>F)`[2]
df_diff <- anova_res$Df[2]
ss_diff <- anova_res$`Sum of Sq`[2]

# Proxy de "sesgo por restricción" del modelo simple:
# si el p-value es pequeño, el modelo sin interacción está mal restringido (más sesgo).
bias_proxy_m4 <- p_val
bias_proxy_m5 <- NA_real_  # el modelo flexible no tiene esa restricción específica

# --- 3) Tabla final ---
results_compare <- data.frame(
  Model = c("No interaction (Model 1)", "With interaction (Model 2)"),
  Parameters_K = c(k_m4, k_m5),          # proxy varianza/complejidad
  Mean_SE = c(mean_se_m4, mean_se_m5),   # proxy varianza (opcional)
  Adj_R2 = c(adjr2_m4, adjr2_m5),
  AIC = c(aic_m4, aic_m5),
  BIC = c(bic_m4, bic_m5),
  RSS = c(rss_m4, rss_m5),
  Bias_proxy_p = c(bias_proxy_m4, bias_proxy_m5), # proxy sesgo por restricción
  F_test = c(NA_real_, F_stat),
  p_value = c(NA_real_, p_val),
  df_added = c(NA_integer_, df_diff),
  SS_added = c(NA_real_, ss_diff)
)

# Formato más bonito
results_compare$Adj_R2 <- round(results_compare$Adj_R2, 4)
results_compare$Mean_SE <- round(results_compare$Mean_SE, 4)
results_compare$AIC <- round(results_compare$AIC, 2)
results_compare$BIC <- round(results_compare$BIC, 2)
results_compare$RSS <- round(results_compare$RSS, 2)
results_compare$Bias_proxy_p <- signif(results_compare$Bias_proxy_p, 3)
results_compare$F_test <- round(results_compare$F_test, 2)
results_compare$p_value <- signif(results_compare$p_value, 3)
results_compare$SS_added <- round(results_compare$SS_added, 2)

gt_tbl <- gt(results_compare) |>
  tab_header(
    title = "Model Comparison: Flexibility Trade-off & Nested F-test",
    subtitle = "Model 1 (No interaction) vs Model 2 (With interaction)"
  ) |>
  cols_label(
    Parameters_K = "K (complexity)",
    Mean_SE = "Mean SE (proxy var.)",
    Adj_R2 = "Adj. R²",
    Bias_proxy_p = "Bias proxy (p)",
    F_test = "F-test",
    p_value = "p-value",
    df_added = "Δdf",
    SS_added = "ΔSS"
  )

gtsave(gt_tbl, filename = "model_comparison_tradeoff.png", path = out_tab)

cat(sprintf("\nGuardado PNG: %s\n", file.path(out_tab, "model_comparison_tradeoff.png")))



cat("\n================================================================================\n")
cat("PASO 4: IMPLIED PEAK AGES \n")
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

boot_peaks <- boot::boot(data, statistic = boot_peak_fun, R = 1000)

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


gt_tbl <- gt(peak_table) |>
  tab_header(title = "Implied Peak Ages by Gender") |>
  fmt_number(columns = everything(), decimals = 2)

gtsave(gt_tbl, filename = "peak_ages_section2.png", path = out_tab)
