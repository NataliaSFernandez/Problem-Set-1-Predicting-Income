# Problem Set 1: Predicción de Ingresos Laborales
## Big Data y Machine Learning para Economía Aplicada

**Equipo 04** 
**Febrero 2026**

---

## Descripción del Proyecto

Este proyecto analiza los ingresos laborales en Bogotá utilizando datos de la Encuesta GEIH 2018. Se implementan tres análisis principales:

1. **Sección 1: Perfil Edad-Ingreso** - Evalúa la teoría de capital humano estimando la relación entre edad e ingresos, identificando la edad de máximo ingreso mediante intervalos de confianza bootstrap.

2. **Sección 2: Brecha Salarial de Género** - Descompone la brecha salarial de género en componentes explicados y no explicados utilizando la descomposición Frisch-Waugh-Lovell.

3. **Sección 3: Predicción de Ingresos** - Evalúa múltiples especificaciones de modelos para predicción fuera de muestra, implementando diagnósticos LOOCV y análisis de influencia.

---

## Estructura del Repositorio

```
Problem-Set-1-Predicting-Income/
│
├── 00_data/
│   ├── raw/                          # Datos GEIH 2018 sin procesar (scraping)
│   └── cleaned/                      # Datos limpios listos para análisis
│       └── data_cleaned.csv
│
├── 01_code/
│   ├── 00_data_scrapper.py          # Scraping de datos GEIH
│   ├── 00_data_cleaning.py          # Limpieza y preprocesamiento
│   ├── 00_descriptive_stats.py      # Estadísticas descriptivas
│   ├── 00_rundirectory.R            # Configuración de directorios en R
│   ├── 01_section1_age_income.R     # Sección 1: Análisis edad-ingreso
│   ├── 02_section2_gendergap_income.R  # Sección 2: Análisis brecha de género
│   └── 03_section3_income_prediction.R # Sección 3: Modelos de predicción
│
├── 02_output/
│   ├── figures/                      # Gráficos y visualizaciones
│   │   ├── 01_section1_age_income/
│   │   ├── 02_section2_gendergap_income/
│   │   ├── 03_section3_prediction/
│   │   └── 05_diagnostic_analysis/
│   └── tables/                       # Tablas de regresiones y resúmenes
│       ├── 01_section1_age_income/
│       ├── 02_section2_gendergap_income/
│       └── 03_section3_prediction/
│
├── requirements.txt                  # Dependencias
├── .gitignore                        # Patrones de archivos ignorados
├── README.md                         # Este archivo
├── 00_rundirectory.sh                        # Ejecutar todos los análisis (Mac/Linux)
└── run_all.ps1                       # Ejecutar todos los análisis (Windows PowerShell)
```

---

## Instrucciones de Instalación

### Requisitos Previos

**Software requerido:**
- R (versión 4.0 o superior)
- Python (versión 3.8 o superior)
- Git (para clonar el repositorio)

### Pasos de Instalación

#### 1. Clonar el repositorio

```bash
git clone https://github.com/NataliaSFernandez/Problem-Set-1-Predicting-Income.git
cd Problem-Set-1-Predicting-Income
```

#### 2. Configurar entorno de Python

**Opción A: Usando pip (recomendado)**
```bash
pip install -r requirements.txt
```

**Opción B: Usando conda**
```bash
conda create -n ps1 python=3.12
conda activate ps1
pip install -r requirements.txt
```

#### 3. Configurar paquetes de R

**Paquetes requeridos:**
- ggplot2
- stargazer
- dplyr
- boot
- webshot
- htmltools
- tidyverse
- broom

**Instalar en R:**
```r
install.packages(c("ggplot2", "stargazer", "dplyr", "boot", 
                   "webshot", "htmltools", "tidyverse", "broom"))

# Instalar phantomjs para webshot (generación de PNG)
webshot::install_phantomjs()
```

---

## Ejecución de los Análisis

### Inicio Rápido: Ejecutar Todo

**Mac/Linux:**
```bash
bash 00_rundirectory.sh
```

**Windows PowerShell:**
```powershell
.\run_all.ps1
```

**Tiempo total estimado:** 20-30 minutos
- Scraping de datos: aproximadamente 2 minutos
- Limpieza de datos: aproximadamente 1 minuto
- Sección 1: aproximadamente 8 minutos (incluye 1000 iteraciones bootstrap)
- Sección 2: aproximadamente 10 minutos (incluye bootstrap para picos por género)
- Sección 3: aproximadamente 15 minutos (incluye LOOCV y análisis de influencia)

---

### Paso a Paso: Ejecutar Scripts Individuales

Si prefiere ejecutar los scripts individualmente:

#### Paso 0: Preparación de Datos

```bash
# Scraping de datos GEIH 2018
python 01_code/00_data_scrapper.py

# Limpieza y preprocesamiento
python 01_code/00_data_cleaning.py

# Generar estadísticas descriptivas (opcional)
python 01_code/00_descriptive_stats.py
```

**Productos:**
- `00_data/raw/geih_2018_completo.csv` (datos sin procesar)
- `00_data/cleaned/data_cleaned.csv` (datos limpios, N=14,764)

---

#### Paso 1: Análisis Perfil Edad-Ingreso

```bash
Rscript 01_code/01_section1_age_income.R
```

**Qué hace:**
- Estima regresiones edad-ingreso incondicionales y condicionales
- Calcula edad de máximo ingreso para ambos modelos
- Genera intervalos de confianza bootstrap (1000 iteraciones, semilla=12345)
- Crea gráficos de perfiles edad-ingreso

**Productos principales:**
- `02_output/tables/01_section1_age_income/age_income_regressions.tex`
- `02_output/tables/01_section1_age_income/peak_age_estimates_both.png`
- `02_output/figures/01_section1_age_income/age_income_profile.png`

**Tiempo de ejecución:** aproximadamente 8 minutos

---

#### Paso 2: Análisis Brecha Salarial de Género

```bash
Rscript 01_code/02_section2_gendergap_income.R
```

**Qué hace:**
- Estima brechas salariales de género incondicionales y condicionales
- Implementa descomposición Frisch-Waugh-Lovell
- Compara errores estándar analíticos vs bootstrap (semilla=123)
- Estima perfiles edad-ingreso específicos por género con interacciones
- Calcula edades de máximo ingreso por género con intervalos de confianza bootstrap

**Productos principales:**
- `02_output/tables/02_section2_gendergap_income/gender_gap_regressions.tex`
- `02_output/figures/02_section2_gendergap_income/gender_profiles.png`
- `02_output/tables/02_section2_gendergap_income/peak_age_by_gender.tex`

**Tiempo de ejecución:** aproximadamente 10 minutos

---

#### Paso 3: Predicción de Ingresos y Diagnósticos

```bash
Rscript 01_code/03_section3_income_prediction.R
```

**Qué hace:**
- Divide datos en entrenamiento (chunks 1-7) y validación (chunks 8-10)
- Estima 9 especificaciones de modelos con complejidad creciente
- Evalúa RMSE fuera de muestra en conjunto de validación
- Implementa Leave-One-Out Cross-Validation (LOOCV) en entrenamiento
- Calcula medidas de influencia
- Diagnostica observaciones con alto error y alta influencia

**Productos principales:**
- `02_output/tables/03_section3_prediction/model_comparison.tex`
- `02_output/tables/03_section3_prediction/rmse_comparison.png`
- `02_output/05_diagnostic_analysis/loocv_influence_all_train.csv`
- `02_output/05_diagnostic_analysis/figures/03_influence_vs_error.png`

**Tiempo de ejecución:** aproximadamente 15 minutos

---

## Resultados Principales

### Sección 1: Perfil Edad-Ingreso

- **Edad de máximo ingreso (incondicional):** 40.6 años [IC 95%: 40.1, 41.2]
- **Edad de máximo ingreso (condicional):** 43.6 años [IC 95%: 42.9, 44.5]
- **Desplazamiento:** 3.0 años (estadísticamente significativo)
- **Interpretación:** Controlar por horas y tipo de empleo desplaza el pico hacia edades mayores, sugiriendo que aproximadamente 15% del efecto edad opera vía oferta laboral

### Sección 2: Brecha Salarial de Género

- **Brecha incondicional:** 25% (mujeres ganan 25% menos que hombres)
- **Brecha condicional:** 15% (después de controlar por edad, horas, tipo de empleo)
- **Componente explicado:** 40% de la brecha total
- **Edad pico - Hombres:** 43 años
- **Edad pico - Mujeres:** 38 años (5 años antes)
- **Interpretación:** La evidencia rechaza el principio de "igual pago por igual trabajo"

### Sección 3: Predicción de Ingresos

- **Mejor modelo RMSE (validación):** 0.58 log-puntos (aproximadamente 58% de error)
- **RMSE baseline:** 0.71 log-puntos (aproximadamente 90% de error)
- **Mejora:** 20% de reducción relativa en error de predicción
- **LOOCV vs Validación:** Similar (0.56 vs 0.58), indicando buena generalización
- **Grupos con alto error:** Altos ingresos, trabajadores independientes, edades extremas

---

## Notas de Reproducibilidad

### Semillas Aleatorias

Todos los análisis utilizan semillas fijas para reproducibilidad:
- **Sección 1:** `set.seed(12345)` para bootstrap
- **Sección 2:** `set.seed(123)` para bootstrap y FWL
- **Sección 3:** Sin muestreo aleatorio (LOOCV determinístico)

### Filtros de Datos

Filtros aplicados (ver `00_data_cleaning.py`):
1. Edad mayor o igual a 18 años
2. Ocupados (oci == 1)
3. Ingreso laboral positivo (y_ingLab_m_ha > 0)

**Retención de muestra:** 14,764 / 32,177 = 45.9%

### Notas Computacionales

- **Iteraciones bootstrap:** 1000 (estándar en la literatura)
- **LOOCV:** Implementado con atajo FWL para eficiencia
- **Medidas de influencia:** Regresores estandarizados para evitar problemas de escala
- **Phantom.js:** Requerido para generación de PNG desde tablas HTML

---

## Dependencias

### Paquetes de Python

Ver `requirements.txt` para lista completa. Paquetes principales:
- pandas - Manipulación de datos
- beautifulsoup4, requests - Web scraping
- scipy, numpy - Computación científica
- matplotlib, seaborn - Visualización
- streamlit - Aplicaciones interactivas (opcional)

### Paquetes de R

- **Core:** ggplot2, dplyr, tidyverse
- **Tablas:** stargazer, broom
- **Bootstrap:** boot
- **Utilidades:** webshot, htmltools

---

## Contacto

Para preguntas o problemas, contactar al Equipo 04 vía la plataforma del curso.

Repositorio: https://github.com/NataliaSFernandez/Problem-Set-1-Predicting-Income

---

## Fuente

- Fuente de datos: GEIH 2018 (Gran Encuesta Integrada de Hogares)
- Curso: Big Data y Machine Learning para Economía Aplicada (MECA 4107)

---

**Última actualización:** 20 de febrero de 2026
