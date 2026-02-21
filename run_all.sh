#!/bin/bash
################################################################################
# run_all.sh - Ejecutar todos los análisis del Problem Set 1
################################################################################
# Descripción:
#   Este script ejecuta todos los scripts de preparación de datos y análisis
#   en el orden correcto para el Problem Set 1: Predicción de Ingresos.
#
# Uso:
#   bash run_all.sh
#
# Requisitos:
#   - Python con paquetes de requirements.txt
#   - R con paquetes requeridos
#   - Conexión a internet (para scraping de datos)
#
# Tiempo estimado de ejecución: 20-30 minutos
################################################################################

# Salir si hay error
set -e

# Códigos de color para salida
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Funciones para imprimir mensajes con color
print_header() {
    echo -e "${BLUE}================================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}================================================================${NC}"
}

print_success() {
    echo -e "${GREEN}[OK] $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}[ADVERTENCIA] $1${NC}"
}

print_error() {
    echo -e "${RED}[ERROR] $1${NC}"
}

# Función para verificar si comando existe
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

################################################################################
# VERIFICACIONES PREVIAS
################################################################################

print_header "VERIFICACIONES PREVIAS"

# Verificar Python
if command_exists python; then
    PYTHON_VERSION=$(python --version 2>&1 | awk '{print $2}')
    print_success "Python encontrado: $PYTHON_VERSION"
else
    print_error "Python no encontrado. Por favor instalar Python"
    exit 1
fi

# Verificar R
if command_exists R; then
    R_VERSION=$(R --version | head -1 | awk '{print $3}')
    print_success "R encontrado: $R_VERSION"
else
    print_error "R no encontrado. Por favor instalar R"
    exit 1
fi

# Verificar Rscript
if command_exists Rscript; then
    print_success "Rscript encontrado"
else
    print_error "Rscript no encontrado. Por favor verificar instalación de R"
    exit 1
fi

echo ""

################################################################################
# INICIAR EJECUCIÓN
################################################################################

print_header "INICIANDO PIPELINE COMPLETO DE ANÁLISIS"
echo "Tiempo total estimado: 20-30 minutos"
echo "Iniciado: $(date '+%Y-%m-%d %H:%M:%S')"
echo ""

START_TIME=$(date +%s)

################################################################################
# PASO 0: PREPARACIÓN DE DATOS
################################################################################

print_header "PASO 0: PREPARACIÓN DE DATOS"

# Paso 0.1: Scraping de Datos
echo "Ejecutando: 00_data_scrapper.py"
echo "Tiempo esperado: aproximadamente 2 minutos"
STEP_START=$(date +%s)

if python 01_code/00_data_scrapper.py; then
    STEP_END=$(date +%s)
    STEP_DURATION=$((STEP_END - STEP_START))
    print_success "Scraping de datos completado en ${STEP_DURATION}s"
else
    print_error "Scraping de datos falló"
    exit 1
fi

echo ""

# Paso 0.2: Limpieza de Datos
echo "Ejecutando: 00_data_cleaning.py"
echo "Tiempo esperado: aproximadamente 1 minuto"
STEP_START=$(date +%s)

if python 01_code/00_data_cleaning.py; then
    STEP_END=$(date +%s)
    STEP_DURATION=$((STEP_END - STEP_START))
    print_success "Limpieza de datos completada en ${STEP_DURATION}s"
else
    print_error "Limpieza de datos falló"
    exit 1
fi

echo ""

# Paso 0.3: Estadísticas Descriptivas (opcional)
echo "Ejecutando: 00_descriptive_stats.py"
echo "Tiempo esperado: aproximadamente 30 segundos"
STEP_START=$(date +%s)

if python 01_code/00_descriptive_stats.py; then
    STEP_END=$(date +%s)
    STEP_DURATION=$((STEP_END - STEP_START))
    print_success "Estadísticas descriptivas completadas en ${STEP_DURATION}s"
else
    print_warning "Estadísticas descriptivas fallaron (no crítico)"
fi

echo ""

################################################################################
# PASO 1: ANÁLISIS PERFIL EDAD-INGRESO
################################################################################

print_header "PASO 1: ANÁLISIS PERFIL EDAD-INGRESO"
echo "Ejecutando: 01_section1_age_income.R"
echo "Tiempo esperado: aproximadamente 8 minutos (incluye 1000 iteraciones bootstrap)"
STEP_START=$(date +%s)

if Rscript 01_code/01_section1_age_income.R; then
    STEP_END=$(date +%s)
    STEP_DURATION=$((STEP_END - STEP_START))
    STEP_MINUTES=$((STEP_DURATION / 60))
    STEP_SECONDS=$((STEP_DURATION % 60))
    print_success "Sección 1 completada en ${STEP_MINUTES}m ${STEP_SECONDS}s"
else
    print_error "Sección 1 falló"
    exit 1
fi

echo ""

################################################################################
# PASO 2: ANÁLISIS BRECHA SALARIAL DE GÉNERO
################################################################################

print_header "PASO 2: ANÁLISIS BRECHA SALARIAL DE GÉNERO"
echo "Ejecutando: 02_section2_gendergap_income.R"
echo "Tiempo esperado: aproximadamente 10 minutos (incluye FWL y bootstrap)"
STEP_START=$(date +%s)

if Rscript 01_code/02_section2_gendergap_income.R; then
    STEP_END=$(date +%s)
    STEP_DURATION=$((STEP_END - STEP_START))
    STEP_MINUTES=$((STEP_DURATION / 60))
    STEP_SECONDS=$((STEP_DURATION % 60))
    print_success "Sección 2 completada en ${STEP_MINUTES}m ${STEP_SECONDS}s"
else
    print_error "Sección 2 falló"
    exit 1
fi

echo ""

################################################################################
# PASO 3: PREDICCIÓN DE INGRESOS Y DIAGNÓSTICOS
################################################################################

print_header "PASO 3: PREDICCIÓN DE INGRESOS Y DIAGNÓSTICOS"
echo "Ejecutando: 03_section3_income_prediction.R"
echo "Tiempo esperado: aproximadamente 15 minutos (incluye LOOCV y análisis de influencia)"
STEP_START=$(date +%s)

if Rscript 01_code/03_section3_income_prediction.R; then
    STEP_END=$(date +%s)
    STEP_DURATION=$((STEP_END - STEP_START))
    STEP_MINUTES=$((STEP_DURATION / 60))
    STEP_SECONDS=$((STEP_DURATION % 60))
    print_success "Sección 3 completada en ${STEP_MINUTES}m ${STEP_SECONDS}s"
else
    print_error "Sección 3 falló"
    exit 1
fi

echo ""

################################################################################
# FINALIZACIÓN
################################################################################

END_TIME=$(date +%s)
TOTAL_DURATION=$((END_TIME - START_TIME))
TOTAL_MINUTES=$((TOTAL_DURATION / 60))
TOTAL_SECONDS=$((TOTAL_DURATION % 60))

print_header "TODOS LOS ANÁLISIS COMPLETADOS EXITOSAMENTE"
echo "Tiempo total de ejecución: ${TOTAL_MINUTES} minutos ${TOTAL_SECONDS} segundos"
echo "Finalizado: $(date '+%Y-%m-%d %H:%M:%S')"
echo ""

print_success "Todos los resultados guardados en:"
echo "  - 00_data/cleaned/data_cleaned.csv"
echo "  - 02_output/figures/"
echo "  - 02_output/tables/"
echo ""

print_success "Puede revisar los resultados o proceder con la preparación de la presentación."
echo ""

################################################################################
# FIN
################################################################################
