################################################################################
# run_all.ps1 - Ejecutar todos los análisis del Problem Set 1 (Windows PowerShell)
################################################################################
# Descripción:
#   Este script ejecuta todos los scripts de preparación de datos y análisis
#   en el orden correcto para el Problem Set 1: Predicción de Ingresos.
#
# Uso:
#   .\run_all.ps1
#
# Requisitos:
#   - Python con paquetes de requirements.txt
#   - R con paquetes requeridos
#   - Conexión a internet (para scraping de datos)
#
# Tiempo estimado de ejecución: 20-30 minutos
################################################################################

# Detener ejecución en caso de error
$ErrorActionPreference = "Stop"

# Función para imprimir con color
function Write-Header {
    param($Message)
    Write-Host "================================================================" -ForegroundColor Blue
    Write-Host $Message -ForegroundColor Blue
    Write-Host "================================================================" -ForegroundColor Blue
}

function Write-Success {
    param($Message)
    Write-Host "[OK] $Message" -ForegroundColor Green
}

function Write-Warning {
    param($Message)
    Write-Host "[ADVERTENCIA] $Message" -ForegroundColor Yellow
}

function Write-Error-Custom {
    param($Message)
    Write-Host "[ERROR] $Message" -ForegroundColor Red
}

# Función para verificar si comando existe
function Test-Command {
    param($CommandName)
    $null -ne (Get-Command $CommandName -ErrorAction SilentlyContinue)
}

################################################################################
# VERIFICACIONES PREVIAS
################################################################################

Write-Header "VERIFICACIONES PREVIAS"

# Verificar Python
if (Test-Command python) {
    $PythonVersion = (python --version 2>&1) -replace "Python ", ""
    Write-Success "Python encontrado: $PythonVersion"
} else {
    Write-Error-Custom "Python no encontrado. Por favor instalar Python"
    exit 1
}

# Verificar R
if (Test-Command R) {
    $RVersion = (R --version 2>&1 | Select-String "R version" | Select-Object -First 1) -replace "R version ", "" -replace " --.*", ""
    Write-Success "R encontrado: $RVersion"
} else {
    Write-Error-Custom "R no encontrado. Por favor instalar R"
    exit 1
}

# Verificar Rscript
if (Test-Command Rscript) {
    Write-Success "Rscript encontrado"
} else {
    Write-Error-Custom "Rscript no encontrado. Por favor verificar instalación de R"
    exit 1
}

Write-Host ""

################################################################################
# INICIAR EJECUCIÓN
################################################################################

Write-Header "INICIANDO PIPELINE COMPLETO DE ANÁLISIS"
Write-Host "Tiempo total estimado: 20-30 minutos"
Write-Host "Iniciado: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
Write-Host ""

$StartTime = Get-Date

################################################################################
# PASO 0: PREPARACIÓN DE DATOS
################################################################################

Write-Header "PASO 0: PREPARACIÓN DE DATOS"

# Paso 0.1: Scraping de Datos
Write-Host "Ejecutando: 00_data_scrapper.py"
Write-Host "Tiempo esperado: aproximadamente 2 minutos"
$StepStart = Get-Date

try {
    python 01_code\00_data_scrapper.py
    if ($LASTEXITCODE -ne 0) { throw "Error en scraping" }
    $StepDuration = ((Get-Date) - $StepStart).TotalSeconds
    Write-Success "Scraping de datos completado en $([math]::Round($StepDuration))s"
} catch {
    Write-Error-Custom "Scraping de datos falló"
    exit 1
}

Write-Host ""

# Paso 0.2: Limpieza de Datos
Write-Host "Ejecutando: 00_data_cleaning.py"
Write-Host "Tiempo esperado: aproximadamente 1 minuto"
$StepStart = Get-Date

try {
    python 01_code\00_data_cleaning.py
    if ($LASTEXITCODE -ne 0) { throw "Error en limpieza" }
    $StepDuration = ((Get-Date) - $StepStart).TotalSeconds
    Write-Success "Limpieza de datos completada en $([math]::Round($StepDuration))s"
} catch {
    Write-Error-Custom "Limpieza de datos falló"
    exit 1
}

Write-Host ""

# Paso 0.3: Estadísticas Descriptivas (opcional)
Write-Host "Ejecutando: 00_descriptive_stats.py"
Write-Host "Tiempo esperado: aproximadamente 30 segundos"
$StepStart = Get-Date

try {
    python 01_code\00_descriptive_stats.py
    $StepDuration = ((Get-Date) - $StepStart).TotalSeconds
    Write-Success "Estadísticas descriptivas completadas en $([math]::Round($StepDuration))s"
} catch {
    Write-Warning "Estadísticas descriptivas fallaron (no crítico)"
}

Write-Host ""

################################################################################
# PASO 1: ANÁLISIS PERFIL EDAD-INGRESO
################################################################################

Write-Header "PASO 1: ANÁLISIS PERFIL EDAD-INGRESO"
Write-Host "Ejecutando: 01_section1_age_income.R"
Write-Host "Tiempo esperado: aproximadamente 8 minutos (incluye 1000 iteraciones bootstrap)"
$StepStart = Get-Date

try {
    Rscript 01_code\01_section1_age_income.R
    if ($LASTEXITCODE -ne 0) { throw "Error en sección 1" }
    $StepDuration = ((Get-Date) - $StepStart).TotalSeconds
    $StepMinutes = [math]::Floor($StepDuration / 60)
    $StepSeconds = [math]::Round($StepDuration % 60)
    Write-Success "Sección 1 completada en ${StepMinutes}m ${StepSeconds}s"
} catch {
    Write-Error-Custom "Sección 1 falló"
    exit 1
}

Write-Host ""

################################################################################
# PASO 2: ANÁLISIS BRECHA SALARIAL DE GÉNERO
################################################################################

Write-Header "PASO 2: ANÁLISIS BRECHA SALARIAL DE GÉNERO"
Write-Host "Ejecutando: 02_section2_gendergap_income.R"
Write-Host "Tiempo esperado: aproximadamente 10 minutos (incluye FWL y bootstrap)"
$StepStart = Get-Date

try {
    Rscript 01_code\02_section2_gendergap_income.R
    if ($LASTEXITCODE -ne 0) { throw "Error en sección 2" }
    $StepDuration = ((Get-Date) - $StepStart).TotalSeconds
    $StepMinutes = [math]::Floor($StepDuration / 60)
    $StepSeconds = [math]::Round($StepDuration % 60)
    Write-Success "Sección 2 completada en ${StepMinutes}m ${StepSeconds}s"
} catch {
    Write-Error-Custom "Sección 2 falló"
    exit 1
}

Write-Host ""

################################################################################
# PASO 3: PREDICCIÓN DE INGRESOS Y DIAGNÓSTICOS
################################################################################

Write-Header "PASO 3: PREDICCIÓN DE INGRESOS Y DIAGNÓSTICOS"
Write-Host "Ejecutando: 03_section3_income_prediction.R"
Write-Host "Tiempo esperado: aproximadamente 15 minutos (incluye LOOCV y análisis de influencia)"
$StepStart = Get-Date

try {
    Rscript 01_code\03_section3_income_prediction.R
    if ($LASTEXITCODE -ne 0) { throw "Error en sección 3" }
    $StepDuration = ((Get-Date) - $StepStart).TotalSeconds
    $StepMinutes = [math]::Floor($StepDuration / 60)
    $StepSeconds = [math]::Round($StepDuration % 60)
    Write-Success "Sección 3 completada en ${StepMinutes}m ${StepSeconds}s"
} catch {
    Write-Error-Custom "Sección 3 falló"
    exit 1
}

Write-Host ""

################################################################################
# FINALIZACIÓN
################################################################################

$TotalDuration = ((Get-Date) - $StartTime).TotalSeconds
$TotalMinutes = [math]::Floor($TotalDuration / 60)
$TotalSeconds = [math]::Round($TotalDuration % 60)

Write-Header "TODOS LOS ANÁLISIS COMPLETADOS EXITOSAMENTE"
Write-Host "Tiempo total de ejecución: $TotalMinutes minutos $TotalSeconds segundos"
Write-Host "Finalizado: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
Write-Host ""

Write-Success "Todos los resultados guardados en:"
Write-Host "  - 00_data\cleaned\data_cleaned.csv"
Write-Host "  - 02_output\figures\"
Write-Host "  - 02_output\tables\"
Write-Host ""

Write-Success "Puede revisar los resultados o proceder con la preparación de la presentación."
Write-Host ""

################################################################################
# FIN
################################################################################
