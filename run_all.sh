#!/bin/bash

################################################################################
# PROBLEM SET 1: PREDICCIÓN DE INGRESOS
# Script de Reproducibilidad Completa
################################################################################
# Este script ejecuta todo el pipeline de análisis desde datos crudos hasta
# resultados finales
# Tiempo estimado: 5-7 minutos
################################################################################

set -e  # Salir si hay error

echo "================================================================================"
echo "PROBLEM SET 1: PREDICCIÓN DE INGRESOS - PIPELINE COMPLETO"
echo "================================================================================"
echo ""

# Verificar que existan datos crudos
if [ ! -f "00_data/raw/geih_2018_completo.csv" ]; then
    echo "ERROR: Datos crudos no encontrados en 00_data/raw/geih_2018_completo.csv"
    echo "Por favor ejecute primero el scraper: python 01_code/01_data_scrapper.py"
    exit 1
fi

################################################################################
# FASE 1: ESPECIFICACIÓN BASELINE (elimina ingresos faltantes, N=7,201)
################################################################################

echo "================================================================================"
echo "FASE 1: ESPECIFICACIÓN BASELINE"
echo "================================================================================"
echo ""

echo "[1/6] Limpieza de datos - Baseline..."
python 01_code/02_data_cleaning.py
echo "Listo. Output: 00_data/cleaned/data_cleaned.csv"
echo ""

echo "[2/6] Estadísticas descriptivas - Baseline..."
python 01_code/03_descriptive_stats.py
echo "Listo. Outputs en 02_output/tables/ y 02_output/figures/"
echo ""

echo "[3/6] Sección 1: Regresiones perfil edad-ingreso - Baseline..."
Rscript 01_code/04_section1_age_income.R
echo "Listo. Outputs en 02_output/tables/ y 02_output/figures/"
echo ""

################################################################################
# FASE 2: ESPECIFICACIÓN IMPUTADA (imputa ingresos faltantes, N=8,153)
################################################################################

echo "================================================================================"
echo "FASE 2: ESPECIFICACIÓN IMPUTADA (ROBUSTEZ)"
echo "================================================================================"
echo ""

echo "[4/6] Limpieza de datos - Imputada..."
python 01_code/02_data_cleaning_imputed.py
echo "Listo. Output: 00_data/cleaned/data_cleaned_imputed.csv"
echo ""

echo "[5/6] Estadísticas descriptivas - Imputada..."
python 01_code/03_descriptive_stats_imputed.py
echo "Listo. Outputs en 02_output/tables/ y 02_output/figures/"
echo ""

echo "[6/6] Sección 1: Regresiones perfil edad-ingreso - Imputada..."
Rscript 01_code/04_section1_age_income_imputed.R
echo "Listo. Outputs en 02_output/tables/ y 02_output/figures/"
echo ""

################################################################################
# SECCIÓN 2: BRECHA SALARIAL DE GÉNERO (POR COMPLETAR)
################################################################################

# Descomentar cuando la Sección 2 esté lista
# echo "================================================================================"
# echo "SECCIÓN 2: BRECHA DE INGRESO LABORAL POR GÉNERO"
# echo "================================================================================"
# echo ""
# echo "[SECCIÓN 2] Ejecutando análisis de brecha de género..."
# Rscript 01_code/05_section2_gender_gap.R
# echo "Listo."
# echo ""

################################################################################
# SECCIÓN 3: PREDICCIÓN DE INGRESOS (POR COMPLETAR)
################################################################################

# Descomentar cuando la Sección 3 esté lista
# echo "================================================================================"
# echo "SECCIÓN 3: PREDICCIÓN DE INGRESO LABORAL"
# echo "================================================================================"
# echo ""
# echo "[SECCIÓN 3] Ejecutando modelos de predicción..."
# Rscript 01_code/06_section3_prediction.R
# echo "Listo."
# echo ""

################################################################################
# RESUMEN
################################################################################

echo "================================================================================"
echo "PIPELINE COMPLETADO EXITOSAMENTE"
echo "================================================================================"
echo ""
echo "Archivos generados:"
echo ""
echo "Datos:"
echo "  - 00_data/cleaned/data_cleaned.csv (N=7,201)"
echo "  - 00_data/cleaned/data_cleaned_imputed.csv (N=8,153)"
echo ""
echo "Figuras:"
echo "  - 02_output/figures/missing_heatmap.png"
echo "  - 02_output/figures/missing_heatmap_imputed.png"
echo "  - 02_output/figures/outliers_income_boxplot.png"
echo "  - 02_output/figures/outliers_income_boxplot_imputed.png"
echo "  - 02_output/figures/income_distribution_gender.png"
echo "  - 02_output/figures/income_distribution_gender_imputed.png"
echo "  - 02_output/figures/age_distribution.png"
echo "  - 02_output/figures/age_distribution_imputed.png"
echo "  - 02_output/figures/hours_distribution_gender.png"
echo "  - 02_output/figures/hours_distribution_gender_imputed.png"
echo "  - 02_output/figures/age_income_profile.png"
echo "  - 02_output/figures/age_income_profile_imputed.png"
echo ""
echo "Tablas:"
echo "  - 02_output/tables/cleaning_summary.tex"
echo "  - 02_output/tables/cleaning_summary_imputed.tex"
echo "  - 02_output/tables/descriptive_stats_general.tex"
echo "  - 02_output/tables/descriptive_stats_general_imputed.tex"
echo "  - 02_output/tables/descriptive_stats_gender.tex"
echo "  - 02_output/tables/descriptive_stats_gender_imputed.tex"
echo "  - 02_output/tables/age_income_regressions.tex"
echo "  - 02_output/tables/age_income_regressions_imputed.tex"
echo "  - 02_output/tables/peak_age_estimates.tex"
echo "  - 02_output/tables/peak_age_estimates_imputed.tex"
echo "  - 02_output/tables/comparison_baseline_imputed.tex"
echo ""
echo "================================================================================"
