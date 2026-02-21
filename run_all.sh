#!/usr/bin/env bash
################################################################################
# run_all.sh - Ejecutar todos los análisis del Problem Set 1 (REPRODUCIBLE)
################################################################################
# - Crea y usa un entorno virtual local (.venv)
# - Instala dependencias desde requirements.txt
# - No depende del comando `python` (usa python3 / .venv)
################################################################################

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_header() {
  echo -e "${BLUE}================================================================${NC}"
  echo -e "${BLUE}$1${NC}"
  echo -e "${BLUE}================================================================${NC}"
}
print_success() { echo -e "${GREEN}[OK] $1${NC}"; }
print_warning() { echo -e "${YELLOW}[ADVERTENCIA] $1${NC}"; }
print_error()   { echo -e "${RED}[ERROR] $1${NC}"; }
command_exists(){ command -v "$1" >/dev/null 2>&1; }

print_header "VERIFICACIONES PREVIAS"

# 1) Requerimos python3 para reproducibilidad
if ! command_exists python3; then
  print_error "python3 no encontrado. Instala Python 3 para continuar."
  exit 1
fi

# 2) requirements.txt es obligatorio en la versión reproducible
if [ ! -f "requirements.txt" ]; then
  print_error "requirements.txt no encontrado en la raíz del proyecto."
  exit 1
fi

# 3) Crear .venv si no existe
if [ ! -x ".venv/bin/python" ]; then
  print_warning "No existe .venv. Creando entorno virtual reproducible..."
  python3 -m venv .venv
fi

# 4) Activar .venv
# shellcheck disable=SC1091
source .venv/bin/activate
PYTHON_CMD=".venv/bin/python"

# 5) Actualizar pip tools e instalar deps
$PYTHON_CMD -m pip install --upgrade pip setuptools wheel >/dev/null
print_success "Instalando dependencias desde requirements.txt"
$PYTHON_CMD -m pip install -r requirements.txt

print_success "Python (venv): $($PYTHON_CMD --version 2>&1)"

# 6) Verificar Rscript
if ! command_exists Rscript; then
  print_error "Rscript no encontrado. Revisa instalación de R."
  exit 1
fi
print_success "Rscript encontrado"

echo ""
print_header "INICIANDO PIPELINE COMPLETO"
START_TIME=$(date +%s)

print_header "PASO 0: PREPARACIÓN DE DATOS"
$PYTHON_CMD 01_code/00_data_scrapper.py
$PYTHON_CMD 01_code/00_data_cleaning.py
$PYTHON_CMD 01_code/00_descriptive_stats.py || print_warning "Descriptivas fallaron (no crítico)"

echo ""
print_header "PASO 1: PERFIL EDAD-INGRESO"
Rscript 01_code/01_section1_age_income.R

echo ""
print_header "PASO 2: BRECHA SALARIAL DE GÉNERO"
Rscript 01_code/02_section2_gendergap_income.R

echo ""
print_header "PASO 3: PREDICCIÓN Y DIAGNÓSTICOS"
Rscript 01_code/03_section3_income_prediction.R

END_TIME=$(date +%s)
TOTAL_DURATION=$((END_TIME - START_TIME))
TOTAL_MINUTES=$((TOTAL_DURATION / 60))
TOTAL_SECONDS=$((TOTAL_DURATION % 60))

echo ""
print_header "PIPELINE COMPLETADO"
echo "Tiempo total: ${TOTAL_MINUTES}m ${TOTAL_SECONDS}s"
echo "Resultados disponibles en 02_output/"
