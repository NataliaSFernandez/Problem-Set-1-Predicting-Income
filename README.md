# Problem Set 1: Predicción de Ingresos Laborales
**MECA 4107 - Big Data y Machine Learning para Economía Aplicada**  
**Universidad de los Andes - Equipo 04**

---

## Objetivo del Proyecto

Este proyecto analiza los determinantes del ingreso laboral en Bogotá usando datos de la Gran Encuesta Integrada de Hogares (GEIH) 2018. El análisis se divide en tres secciones:

1. **Perfil Edad-Ingreso**: Estimar el perfil de ingresos laborales a lo largo del ciclo de vida, identificando la edad de máximo ingreso tanto incondicional como condicionalmente a controles laborales.

2. **Brecha Salarial de Género**: Cuantificar la brecha de ingresos entre hombres y mujeres, descomponiéndola mediante el teorema de Frisch-Waugh-Lovell (FWL) para distinguir entre brecha incondicional y brecha condicional ajustando por características observables.

3. **Predicción de Ingresos**: Desarrollar y evaluar modelos predictivos de ingreso laboral utilizando validación fuera de muestra (train/validation split) y Leave-One-Out Cross-Validation (LOOCV), con análisis de influencia y diagnóstico de errores de predicción para aplicaciones de política tributaria.

---

## Estructura del Repositorio

```
Problem-Set-1-Predicting-Income/
│
├── 00_data/
│   ├── raw/                        # Datos crudos descargados (no versionados)
│   └── cleaned/
│       └── data_cleaned.csv        # Dataset limpio (N=14,764)
│
├── 01_code/
│   ├── 00_data_scrapper.py         # Web scraping de GEIH 2018
│   ├── 00_data_cleaning.py         # Limpieza y preparación de datos
│   ├── 00_descriptive_stats.py     # Estadísticas descriptivas
│   ├── 00_rundirectory.R           # Ejecuta solo scripts R (uso interno)
│   ├── 01_section1_age_income.R    # Sección 1: Perfil edad-ingreso
│   ├── 02_section2_gendergap_income.R  # Sección 2: Brecha de género
│   └── 03_section3_income_prediction.R # Sección 3: Predicción
│
├── 02_output/
│   ├── figures/                    # Gráficos generados
│   │   ├── 00_descriptive_stats/
│   │   ├── 01_section1_age_income/
│   │   ├── 02_section2_gendergap_income/
│   │   └── 03_section3_prediction/
│   ├── tables/                     # Tablas generadas
│   │   ├── 00_descriptive_stats/
│   │   ├── 01_section1_age_income/
│   │   ├── 02_section2_gendergap_income/
│   │   └── 03_section3_prediction/
│   └── 05_diagnostic_analysis/     # Diagnósticos LOOCV
│
├── 00_rundirectory.sh              # ⭐ Script principal - ejecuta pipeline completo
├── README.md                       # Este archivo
├── requirements.txt                # Dependencias Python
└── .gitignore                      # Archivos excluidos de Git
```

---

## Requisitos del Sistema

### Software Necesario

- **Python** 3.8 o superior
- **R** 4.0 o superior
- **Git** (para clonar el repositorio)
- **Conexión a internet** (para el scraping inicial de datos)

### Paquetes Python

Instalar con pip:

```bash
pip install pandas beautifulsoup4 requests scipy numpy matplotlib seaborn streamlit
```

O usando el archivo de requisitos:

```bash
pip install -r requirements.txt
```

### Paquetes R

Instalar en R:

```r
install.packages(c("ggplot2", "stargazer", "dplyr", "boot", "webshot", 
                   "htmltools", "tidyverse", "broom", "gt", "splines"))
```

**Nota importante**: El script instala automáticamente `phantomjs` para la generación de imágenes de tablas (`webshot::install_phantomjs()`). Esto solo ocurre la primera vez que se ejecuta.

---

## Cómo Ejecutar el Análisis Completo

### Paso 1: Clonar el Repositorio

```bash
git clone https://github.com/NataliaSFernandez/Problem-Set-1-Predicting-Income.git
cd Problem-Set-1-Predicting-Income
```

### Paso 2: Instalar Dependencias

**Python:**

```bash
pip install -r requirements.txt
```

**R:**

```r
install.packages(c("ggplot2", "stargazer", "dplyr", "boot", "webshot", 
                   "htmltools", "tidyverse", "broom", "gt", "splines"),
                 repos = "https://cloud.r-project.org")
```

### Paso 3: Ejecutar el Pipeline Completo

Desde la raíz del repositorio:

```bash
bash 00_rundirectory.sh
```

**Este script ejecuta automáticamente:**

1. Scraping de datos GEIH 2018 (Chunks 1-10)
2. Limpieza y preparación del dataset
3. Generación de estadísticas descriptivas
4. Sección 1: Estimación de perfiles edad-ingreso con bootstrap
5. Sección 2: Análisis de brecha de género con FWL
6. Sección 3: Modelos de predicción y diagnósticos LOOCV

**Tiempo estimado de ejecución:** 10-30 minutos (dependiendo del hardware)

**Salida en consola:** El script muestra mensajes de progreso en color (azul para encabezados, verde para éxitos, rojo para errores) y reporta el tiempo de ejecución de cada etapa.

### Opción Alternativa: Ejecución Paso a Paso

Si se desea ejecutar cada etapa manualmente:

```bash
# Paso 0: Obtención y preparación de datos
python 01_code/00_data_scrapper.py          # ~2 minutos
python 01_code/00_data_cleaning.py          # ~1 minuto
python 01_code/00_descriptive_stats.py      # ~30 segundos

# Paso 1: Sección 1 - Perfil edad-ingreso
Rscript 01_code/01_section1_age_income.R    # ~8 minutos (1000 bootstrap iterations)

# Paso 2: Sección 2 - Brecha de género
Rscript 01_code/02_section2_gendergap_income.R  # ~10 minutos (FWL + bootstrap)

# Paso 3: Sección 3 - Predicción
Rscript 01_code/03_section3_income_prediction.R # ~3 minutos (LOOCV analítico)
```

---

## Decisiones de Limpieza de Datos

### Filtros Aplicados

1. **Edad ≥ 18 años**: Población adulta (elimina 7,609 obs)
2. **Ocupados (ocu == 1)**: Solo personas empleadas (elimina 8,026 obs)
3. **Ingreso laboral > 0**: Excluye ingresos cero o negativos (elimina 1,778 obs)

**Muestra final:** 14,764 observaciones (45.9% de la muestra original de 32,177)

### Manejo de Missing Values

- **Sexo (sex)**: 0 missings - variable crítica para análisis de género
- **Horas trabajadas**: 0 missings después de filtros
- **Nivel educativo (maxEducLevel)**: 1 missing imputado con la moda (nivel 7)

### Tratamiento de Outliers

- **Detección**: Método IQR identifica 1,825 outliers superiores (12.4% de la muestra)
- **Decisión**: **Se mantienen** todos los outliers en el análisis
- **Solución**: Usar transformación logarítmica `log(y_total_m)` en todas las regresiones, que reduce automáticamente la influencia de valores extremos

### Variables Derivadas

- `log_income`: log(y_total_m) - variable dependiente principal
- `age_squared`: edad² - para capturar no-linealidades en el ciclo de vida
- `female`: indicadora (1=mujer, 0=hombre)
- `potExp`: experiencia potencial = edad - años_educación - 6
- Splines cúbicos en edad (Sección 3)
- Interacciones: edad × género, educación × formalidad, educación × estrato (Sección 3)

---

## Mapeo: Slides → Outputs del Código

### Sección 1: Perfil Edad-Ingreso (age_equipo_04.pdf)

| Slide | Contenido | Archivo Generado |
|-------|-----------|------------------|
| 3 | Gráfico: Perfil edad-ingreso observado | `02_output/figures/01_section1_age_income/age_income_profile.png` |
| 4 | Tabla: Regresiones incondicional y condicional | `02_output/tables/01_section1_age_income/age_income_regressions.png` |
| 4 | Tabla LaTeX: Regresiones | `02_output/tables/01_section1_age_income/age_income_regressions.tex` |
| 5 | Tabla: Peak ages con bootstrap CI | `02_output/tables/01_section1_age_income/peak_age_estimates_both.png` |
| 5 | Tabla LaTeX: Peak ages | `02_output/tables/01_section1_age_income/peak_age_estimates_both.tex` |

**Resultados clave reportados:**
- Pico incondicional: 40.6 años [40.1, 41.2]
- Pico condicional: 43.6 años [42.9, 44.5]
- Desplazamiento: 3.0 años (significativo, intervalos no se superponen)

---

### Sección 2: Brecha Salarial de Género (gap_equipo_04.pdf)

| Slide | Contenido | Archivo Generado |
|-------|-----------|------------------|
| 3 | Tabla: Comparación modelos FWL (M1-M5) | `02_output/tables/02_section2_gendergap_income/model_comparison.png` |
| 4 | Tabla: Brecha incondicional vs condicional | `02_output/tables/02_section2_gendergap_income/gender_gap_table.png` |
| 5 | Gráfico: Perfiles edad-ingreso por género (sin interacción) | `02_output/figures/02_section2_gendergap_income/age_labor_no_interaction.png` |
| 6 | Gráfico: Perfiles con interacción edad×género | `02_output/figures/02_section2_gendergap_income/age_labor_interaction.png` |
| 7 | Tabla: Trade-off R² vs AIC entre modelos | `02_output/tables/02_section2_gendergap_income/model_comparison_tradeoff.png` |
| 8 | Tabla: Peak ages por género con bootstrap CI | `02_output/tables/02_section2_gendergap_income/peak_ages_section2.png` |

**Resultados clave reportados:**
- Brecha incondicional: -23.8% (exp(-0.238) - 1)
- Brecha condicional (Modelo 5): -13.5% (exp(-0.135) - 1)
- Componente explicado por características observables: 43% ((0.238-0.135)/0.238)
- Peak edad hombres: 43 años [42, 44]
- Peak edad mujeres: 40 años [38, 41]

---

### Sección 3: Predicción de Ingresos (pred_equipo_04.pdf)

| Slide | Contenido | Archivo Generado |
|-------|-----------|------------------|
| 4 | Tabla: Especificaciones de los 9 modelos | Generada en script (mostrada en consola) |
| 5 | Gráfico: Comparación RMSE de todos los modelos | `02_output/figures/03_section3_prediction/rmse_comparison.png` |
| 6 | Especificación del mejor modelo (Alt 5) | Documentado en README y consola |
| 7 | Gráfico: LOOCV vs Validation RMSE | Generado en presentación desde datos de consola |
| 8 | Diagnóstico: Errores, leverage, influencia | `02_output/05_diagnostic_analysis/figures/01_loocv_error_hist.png` |
| 8 | | `02_output/05_diagnostic_analysis/figures/02_loocv_leverage.png` |
| 8 | | `02_output/05_diagnostic_analysis/figures/03_influence_vs_error.png` |
| 8 | | `02_output/05_diagnostic_analysis/figures/04_top50_difficult.png` |

**Archivos adicionales generados (Sección 3):**
- `02_output/tables/03_section3_prediction/model_comparison.tex` - Tabla LaTeX de modelos
- `02_output/tables/03_section3_prediction/coefficients_models.csv` - Coeficientes de todos los modelos (513 coefs)
- `02_output/05_diagnostic_analysis/loocv_vs_validation_summary.csv` - Resumen LOOCV
- `02_output/05_diagnostic_analysis/top100_loocv_errors.csv` - Top 100 errores LOOCV
- `02_output/05_diagnostic_analysis/top100_influence_beta_sq.csv` - Top 100 observaciones influyentes
- `02_output/05_diagnostic_analysis/loocv_influence_all_train.csv` - Dataset completo con diagnósticos (10,263 obs)

**Resultados clave reportados:**
- Mejor modelo: Alt 5 (Integrado)
- RMSE validation: 0.5602
- Mejora vs baseline (Base 1): 37.2%
- LOOCV RMSE (training): 0.5687
- Gap LOOCV-Validation: 0.008 (mínimo → no overfitting)

---

## Lista Completa de Outputs Generados

### Datos Limpios

- `00_data/cleaned/data_cleaned.csv` (14,764 observaciones × 183 variables)

### Figuras - Estadísticas Descriptivas

- `02_output/figures/00_descriptive_stats/income_distribution_gender.png`
- `02_output/figures/00_descriptive_stats/age_distribution.png`
- `02_output/figures/00_descriptive_stats/hours_distribution_gender.png`
- `02_output/figures/missing_heatmap.png`
- `02_output/figures/outliers_income_boxplot.png`

### Tablas - Estadísticas Descriptivas

- `02_output/tables/00_descriptive_stats/descriptive_stats_general.tex`
- `02_output/tables/00_descriptive_stats/descriptive_stats_gender.tex`
- `02_output/tables/00_descriptive_stats/descriptive_table_gender.png`
- `02_output/tables/cleaning_summary.tex`

### Figuras - Sección 1

- `02_output/figures/01_section1_age_income/age_income_profile.png`

### Tablas - Sección 1

- `02_output/tables/01_section1_age_income/age_income_regressions.tex`
- `02_output/tables/01_section1_age_income/age_income_regressions.png`
- `02_output/tables/01_section1_age_income/peak_age_estimates.tex`
- `02_output/tables/01_section1_age_income/peak_age_estimates.png`
- `02_output/tables/01_section1_age_income/peak_age_estimates_both.tex`
- `02_output/tables/01_section1_age_income/peak_age_estimates_both.png`

### Figuras - Sección 2

- `02_output/figures/02_section2_gendergap_income/age_labor_no_interaction.png`
- `02_output/figures/02_section2_gendergap_income/age_labor_interaction.png`
- `02_output/figures/02_section2_gendergap_income/Age_labor_income_profiles.png` (alias)
- `02_output/figures/02_section2_gendergap_income/05_age_income_profiles_with_peaks.png`

### Tablas - Sección 2

- `02_output/tables/02_section2_gendergap_income/model_comparison.png`
- `02_output/tables/02_section2_gendergap_income/02_gender_gap_table.png`
- `02_output/tables/02_section2_gendergap_income/gender_gap_table.png`
- `02_output/tables/02_section2_gendergap_income/model_comparison_tradeoff.png`
- `02_output/tables/02_section2_gendergap_income/peak_ages_section2.png`
- `02_output/tables/02_section2_gendergap_income/05_peak_ages_section2.tex`

### Figuras - Sección 3

- `02_output/figures/03_section3_prediction/rmse_comparison.png`

### Tablas - Sección 3

- `02_output/tables/03_section3_prediction/model_comparison.tex`
- `02_output/tables/03_section3_prediction/coefficients_models.csv`

### Diagnósticos - Sección 3 (LOOCV)

**Figuras:**
- `02_output/05_diagnostic_analysis/figures/01_loocv_error_hist.png`
- `02_output/05_diagnostic_analysis/figures/02_loocv_leverage.png`
- `02_output/05_diagnostic_analysis/figures/03_influence_vs_error.png`
- `02_output/05_diagnostic_analysis/figures/04_top50_difficult.png`

**CSVs:**
- `02_output/05_diagnostic_analysis/loocv_vs_validation_summary.csv`
- `02_output/05_diagnostic_analysis/top100_loocv_errors.csv`
- `02_output/05_diagnostic_analysis/top100_influence_beta_sq.csv`
- `02_output/05_diagnostic_analysis/loocv_influence_all_train.csv`

---

## Notas de Reproducibilidad

### Semillas Aleatorias

- **Sección 1**: `set.seed(12345)` para bootstrap de peak age
- **Sección 2**: `set.seed(123)` para bootstrap de brecha de género
- **Sección 3**: Determinístico (LOOCV analítico sin componente aleatorio)

### Tiempos de Ejecución

En una MacBook Pro con procesador M1 (10 cores):

- Scraping: ~2 minutos
- Limpieza: ~1 minuto
- Sección 1: ~45 segundos (1000 bootstrap iterations)
- Sección 2: ~6 minutos (FWL + bootstrap 1000 iterations)
- Sección 3: ~3 segundos (LOOCV analítico)

**Total:** ~10 minutos

Los tiempos pueden variar según el hardware. En equipos con menos cores o memoria, esperar 20-30 minutos.

### Validación de Resultados

Para verificar que el pipeline se ejecutó correctamente:

1. Verificar que `00_data/cleaned/data_cleaned.csv` tiene 14,764 filas
2. Verificar que existen todos los archivos listados en la sección "Lista Completa de Outputs"
3. Abrir `02_output/figures/03_section3_prediction/rmse_comparison.png` - debe mostrar Alt 5 como el mejor modelo
4. Verificar en consola que no hay mensajes `[ERROR]` en rojo

---

## Información Técnica Adicional

### División de Muestra (Sección 3)

- **Training set**: Chunks 1-7 → 10,263 observaciones
- **Validation set**: Chunks 8-10 → 4,501 observaciones
- División secuencial para evitar data leakage

### Métodos Implementados

**Sección 1:**
- Regresión OLS con términos cuadráticos en edad
- Bootstrap percentil para intervalos de confianza del peak age
- Método del vértice de parábola para calcular edad de máximo ingreso

**Sección 2:**
- Teorema Frisch-Waugh-Lovell (FWL) para descomposición de coeficientes
- Bootstrap para errores estándar robustos
- Modelo de interacción edad × género para análisis de heterogeneidad

**Sección 3:**
- 9 especificaciones (4 base + 5 alternativas)
- LOOCV calculado analíticamente usando matriz sombrero: `e_i / (1 - h_ii)`
- Influencia en coeficientes: `||β̂₍₋ᵢ₎ - β̂||₂` sobre regresores estandarizados (descomposición FWL)
- Splines cúbicos naturales con diferentes grados de libertad

### Software y Versiones

El código ha sido probado con:
- Python 3.12.4
- R 4.5.1
- macOS Sonoma 14.5

Debería funcionar con versiones:
- Python ≥ 3.8
- R ≥ 4.0

---

## Solución de Problemas Comunes

### Error: "Python not found"

Verificar instalación de Python:

```bash
python --version
# o
python3 --version
```

Si no está instalado, descargar desde [python.org](https://www.python.org/downloads/).

### Error: "Rscript not found"

Verificar que R y Rscript estén en el PATH:

```bash
R --version
Rscript --version
```

Si no están disponibles, reinstalar R desde [r-project.org](https://www.r-project.org/).

### Error: "Chrome debugging port not open"

Este error ocurría con la librería `webshot2`. El código ha sido actualizado para usar `webshot` con `phantomjs`, que se instala automáticamente la primera vez que se ejecuta el script.

Si persiste el error:

```r
# En R
webshot::install_phantomjs()
```

### Error: Missing packages

Si faltan paquetes de R, instalarlos manualmente:

```r
install.packages(c("ggplot2", "stargazer", "dplyr", "boot", "webshot", 
                   "htmltools", "tidyverse", "broom", "gt", "splines"),
                 repos = "https://cloud.r-project.org")
```

Para Python:

```bash
pip install -r requirements.txt
```

### Error: "Permission denied" al ejecutar 00_rundirectory.sh

Dar permisos de ejecución:

```bash
chmod +x 00_rundirectory.sh
```

---

## Contribuciones del Equipo

- **Daniela Solano**: Limpieza de datos, Sección 2, documentación
- **Jonathan Melo**: Sección 1, bootstrap, presentación
- **Natalia Suescún**: Sección 3, LOOCV, diagnósticos

Todos los miembros contribuyeron a la revisión del código, testing y preparación de presentaciones.

Ver historial completo de commits:

```bash
git log --oneline --all
```

---

## Licencia y Uso Académico

Este proyecto fue desarrollado como parte del curso MECA 4107 - Big Data y Machine Learning para Economía Aplicada en la Universidad de los Andes.

El código está disponible públicamente con fines de replicación y aprendizaje. Si se utiliza este código o metodología, favor citar:

> Solano, D., Melo, J., Suescún, N. (2026). Problem Set 1: Predicción de Ingresos Laborales. MECA 4107, Universidad de los Andes.

---

## Contacto

Para preguntas sobre el código o metodología:

- Repositorio: [https://github.com/NataliaSFernandez/Problem-Set-1-Predicting-Income](https://github.com/NataliaSFernandez/Problem-Set-1-Predicting-Income)
- Issues: Abrir un issue en el repositorio de GitHub

---

**Última actualización:** Febrero 21, 2026  
**Versión:** 1.0
