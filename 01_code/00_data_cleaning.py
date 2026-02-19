"""
==============================================================================
PROBLEM SET 1: PREDICTING INCOME
Script 02: Data Cleaning
==============================================================================
OBJETIVO: Limpiar datos GEIH 2018 Bogotá

INPUTS:  00_data/raw/geih_2018_completo.csv (16,089 obs, 178 vars)
OUTPUTS: 
  - 00_data/cleaned/data_cleaned.csv (datos limpios)
  - 02_output/figures/missing_heatmap.png
  - 02_output/figures/outliers_income_boxplot.png
  - 02_output/tables/cleaning_summary.tex

METODOLOGÍA:
  - Basada en Cuadernos 1 (Missing Values) y 2 (Outliers)
  - Cada decisión justificada
==============================================================================
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
from scipy import stats

# Configuración de visualización
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")

# Crear directorios de output si no existen
os.makedirs("02_output/figures", exist_ok=True)
os.makedirs("02_output/tables", exist_ok=True)
os.makedirs("00_data/cleaned", exist_ok=True)

print("="*80)
print("LIMPIEZA DE DATOS GEIH 2018 - PROBLEM SET 1")
print("="*80)

# ==============================================================================
# PASO 1: CARGAR DATOS CRUDOS
# ==============================================================================

print("\n" + "="*80)
print("PASO 1: CARGAR DATOS CRUDOS")
print("="*80)

data_raw = pd.read_csv("00_data/raw/geih_2018_completo.csv")

print(f"\nDatos cargados exitosamente")
print(f"  Observaciones: {data_raw.shape[0]:,}")
print(f"  Variables: {data_raw.shape[1]}")

# Guardar dimensiones originales para comparación posterior
n_original = len(data_raw)
vars_original = data_raw.shape[1]

# ==============================================================================
# PASO 2: ANÁLISIS DE MISSING VALUES - DATOS CRUDOS (ANTES DE FILTROS)
# ==============================================================================

print("\n" + "="*80)
print("PASO 2: ANÁLISIS DE MISSING VALUES - DATOS CRUDOS")
print("="*80)

# Variables que necesitamos según el Problem Set
variables_clave = {
    # Outcome
    'y_total_m': 'Ingreso laboral mensual total',
    
    # Demografía
    'age': 'Edad',
    'sex': 'Sexo (0=mujer, 1=hombre)',
    
    # Capital humano
    'maxEducLevel': 'Nivel educativo máximo',
    
    # Empleo/Firma
    'formal': 'Formalidad (seguridad social)',
    'oficio': 'Ocupación',
    'p6240': 'Sector/actividad económica',
    'p6426': 'Tenure (tiempo en la firma)',
    'relab': 'Tipo de relación laboral',
    'sizeFirm': 'Tamaño de la firma',
    
    # Oferta laboral
    'totalHoursWorked': 'Total horas trabajadas semana anterior',
    
    # Temporal
    'mes': 'Mes de la entrevista',
    
    # Auxiliar para filtros
    'ocu': 'Ocupado (1=sí, 0=no)'
}

# Calcular missings en variables clave ANTES de filtros
missing_analysis_raw = pd.DataFrame({
    'Variable': list(variables_clave.keys()),
    'Missing_Count': [data_raw[var].isna().sum() if var in data_raw.columns else 0 
                      for var in variables_clave.keys()],
    'Missing_Pct': [(data_raw[var].isna().sum() / len(data_raw) * 100) if var in data_raw.columns else 0
                    for var in variables_clave.keys()]
})

print("\nMissing values en DATOS CRUDOS (antes de filtros):")
print(missing_analysis_raw.to_string(index=False))

# Visualizar patrón de missings en DATOS CRUDOS
print("\nGenerando heatmap de missings (datos crudos)...")
fig, ax = plt.subplots(figsize=(12, 8))
vars_to_plot = [v for v in variables_clave.keys() if v in data_raw.columns]
missing_matrix = data_raw[vars_to_plot].isna()

sns.heatmap(missing_matrix.T, 
            cbar=True, 
            cmap='YlOrRd',
            yticklabels=vars_to_plot,
            ax=ax)
ax.set_title('Patrón de Missing Values - Variables Clave (DATOS CRUDOS)', fontsize=14, fontweight='bold')
ax.set_xlabel('Observaciones (sample)', fontsize=12)
ax.set_ylabel('Variables', fontsize=12)
plt.tight_layout()
plt.savefig("02_output/figures/00_data_cleaning/missing_heatmap.png", dpi=300, bbox_inches='tight')
plt.close()
print("  Guardado: 02_output/figures/missing_heatmap.png")

# ==============================================================================
# PASO 3: FILTROS DE OBSERVACIONES
# ==============================================================================

print("\n" + "="*80)
print("PASO 3: FILTROS DE OBSERVACIONES")
print("="*80)

print("\nCriterios según Problem Set:")
print("  - Edad >= 18 años")
print("  - Empleados (ocu == 1)")
print("  - Ingreso laboral > 0")

# Crear copia para no modificar raw
data_filtered = data_raw.copy()

# FILTRO 1: Edad >= 18
print(f"\nAntes del filtro de edad: {len(data_filtered):,} observaciones")
data_filtered = data_filtered[data_filtered['age'] >= 18].copy()
print(f"Después de age >= 18: {len(data_filtered):,} observaciones")
print(f"  Eliminadas: {n_original - len(data_filtered):,} obs (menores de 18)")

# FILTRO 2: Empleados (ocu == 1)
n_before = len(data_filtered)
if 'ocu' in data_filtered.columns:
    data_filtered = data_filtered[data_filtered['ocu'] == 1].copy()
    print(f"Después de filtro empleados: {len(data_filtered):,} observaciones")
    print(f"  Eliminadas: {n_before - len(data_filtered):,} obs (no empleados)")
else:
    print("  Variable 'ocu' no encontrada, saltando filtro")

# FILTRO 3: Ingreso > 0
n_before = len(data_filtered)
data_filtered = data_filtered[data_filtered['y_total_m'] > 0].copy()
print(f"Después de ingreso > 0: {len(data_filtered):,} observaciones")
print(f"  Eliminadas: {n_before - len(data_filtered):,} obs (ingresos cero/negativos)")

print(f"\nFiltros aplicados. N final: {len(data_filtered):,} observaciones")

# ==============================================================================
# PASO 4: ANÁLISIS DE MISSING VALUES POST-FILTROS
# ==============================================================================

print("\n" + "="*80)
print("PASO 4: ANÁLISIS DE MISSING VALUES POST-FILTROS")
print("="*80)

# Calcular missings después de filtros
missing_analysis_filtered = pd.DataFrame({
    'Variable': list(variables_clave.keys()),
    'Missing_Count': [data_filtered[var].isna().sum() if var in data_filtered.columns else 0 
                      for var in variables_clave.keys()],
    'Missing_Pct': [(data_filtered[var].isna().sum() / len(data_filtered) * 100) if var in data_filtered.columns else 0
                    for var in variables_clave.keys()]
})

print("\nMissing values DESPUÉS de filtros:")
print(missing_analysis_filtered.to_string(index=False))

# ==============================================================================
# PASO 5: IMPUTACIÓN DE MISSING VALUES
# ==============================================================================

print("\n" + "="*80)
print("PASO 5: IMPUTACIÓN DE MISSING VALUES")
print("="*80)

# DECISIÓN 1: Eliminar observaciones sin sexo
n_before = len(data_filtered)
if 'sex' in data_filtered.columns:
    data_filtered = data_filtered[data_filtered['sex'].notna()].copy()
    print(f"\n1. Sexo (sex): Eliminadas {n_before - len(data_filtered)} obs sin sexo")
    print(f"   Justificación: Variable crítica para análisis de brecha de género")

# DECISIÓN 2: Imputar horas trabajadas con mediana por sexo
if 'totalHoursWorked' in data_filtered.columns:
    missing_hours = data_filtered['totalHoursWorked'].isna().sum()
    if missing_hours > 0:
        # Mediana por género
        median_female = data_filtered[data_filtered['sex']==0]['totalHoursWorked'].median()
        median_male = data_filtered[data_filtered['sex']==1]['totalHoursWorked'].median()
        
        # Imputar
        data_filtered.loc[(data_filtered['sex']==0) & (data_filtered['totalHoursWorked'].isna()), 
                         'totalHoursWorked'] = median_female
        data_filtered.loc[(data_filtered['sex']==1) & (data_filtered['totalHoursWorked'].isna()), 
                         'totalHoursWorked'] = median_male
        
        print(f"\n2. Horas trabajadas: Imputadas {missing_hours} obs")
        print(f"   Método: Mediana por sexo")
        print(f"   - Mujeres: {median_female:.1f} horas")
        print(f"   - Hombres: {median_male:.1f} horas")
    else:
        print(f"\n2. Horas trabajadas: 0 missings, no requiere imputación")

# DECISIÓN 3: Imputar variables categóricas con moda
categorical_vars = ['relab', 'sizeFirm', 'maxEducLevel', 'oficio', 'p6240']
for var in categorical_vars:
    if var in data_filtered.columns:
        missing_count = data_filtered[var].isna().sum()
        if missing_count > 0:
            moda = data_filtered[var].mode()[0]
            data_filtered[var].fillna(moda, inplace=True)
            print(f"\n3. {var}: Imputadas {missing_count} obs con moda = {moda}")
            print(f"   Método: Moda (valor más frecuente)")

# DECISIÓN 4: Variables numéricas continuas (tenure) - mediana
if 'p6426' in data_filtered.columns:
    missing_count = data_filtered['p6426'].isna().sum()
    if missing_count > 0:
        median_tenure = data_filtered['p6426'].median()
        data_filtered['p6426'].fillna(median_tenure, inplace=True)
        print(f"\n4. Tenure (p6426): Imputadas {missing_count} obs con mediana = {median_tenure:.1f}")
        print(f"   Método: Mediana")

print(f"\nImputación completada. N actual: {len(data_filtered):,} observaciones")

# ==============================================================================
# PASO 6: DETECCIÓN Y TRATAMIENTO DE OUTLIERS
# ==============================================================================

print("\n" + "="*80)
print("PASO 6: ANÁLISIS DE OUTLIERS - INGRESO LABORAL")
print("="*80)

# Estadísticas del ingreso
print("\nEstadísticas de y_total_m (ingreso):")
print(f"  Media: ${data_filtered['y_total_m'].mean():,.0f}")
print(f"  Mediana: ${data_filtered['y_total_m'].median():,.0f}")
print(f"  Desv. Std: ${data_filtered['y_total_m'].std():,.0f}")
print(f"  Mínimo: ${data_filtered['y_total_m'].min():,.0f}")
print(f"  Máximo: ${data_filtered['y_total_m'].max():,.0f}")

# Método IQR
Q1 = data_filtered['y_total_m'].quantile(0.25)
Q3 = data_filtered['y_total_m'].quantile(0.75)
IQR = Q3 - Q1

lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR

outliers_lower = (data_filtered['y_total_m'] < lower_bound).sum()
outliers_upper = (data_filtered['y_total_m'] > upper_bound).sum()

print(f"\nDetección de outliers (Método IQR):")
print(f"  Q1 (25%): ${Q1:,.0f}")
print(f"  Q3 (75%): ${Q3:,.0f}")
print(f"  IQR: ${IQR:,.0f}")
print(f"  Lower bound: ${lower_bound:,.0f}")
print(f"  Upper bound: ${upper_bound:,.0f}")
print(f"  Outliers inferiores: {outliers_lower}")
print(f"  Outliers superiores: {outliers_upper}")
print(f"  Total outliers: {outliers_lower + outliers_upper} ({(outliers_lower + outliers_upper)/len(data_filtered)*100:.1f}%)")

# Percentiles altos
p95 = data_filtered['y_total_m'].quantile(0.95)
p99 = data_filtered['y_total_m'].quantile(0.99)
print(f"\nPercentiles:")
print(f"  P95: ${p95:,.0f}")
print(f"  P99: ${p99:,.0f}")

# DECISIÓN: NO eliminar outliers
print(f"\nDECISIÓN: Mantener outliers")
print(f"  Solución: Usar transformación logarítmica en regresiones")

# Crear flag de outlier para análisis posterior
data_filtered['is_outlier_income'] = ((data_filtered['y_total_m'] < lower_bound) | 
                                       (data_filtered['y_total_m'] > upper_bound)).astype(int)

# Visualizar distribución y outliers
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# Boxplot
axes[0].boxplot(data_filtered['y_total_m'], vert=True)
axes[0].set_ylabel('Ingreso Mensual (COP)', fontsize=12)
axes[0].set_title('Boxplot - Ingreso Laboral Mensual', fontsize=14, fontweight='bold')
axes[0].grid(True, alpha=0.3)

# Histograma
axes[1].hist(data_filtered['y_total_m'], bins=50, edgecolor='black', alpha=0.7)
axes[1].axvline(data_filtered['y_total_m'].mean(), color='red', linestyle='--', 
                linewidth=2, label=f'Media: ${data_filtered["y_total_m"].mean():,.0f}')
axes[1].axvline(data_filtered['y_total_m'].median(), color='green', linestyle='--', 
                linewidth=2, label=f'Mediana: ${data_filtered["y_total_m"].median():,.0f}')
axes[1].set_xlabel('Ingreso Mensual (COP)', fontsize=12)
axes[1].set_ylabel('Frecuencia', fontsize=12)
axes[1].set_title('Distribución de Ingreso Laboral', fontsize=14, fontweight='bold')
axes[1].legend()
axes[1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig("02_output/figures/00_data_cleaning/outliers_income_boxplot.png", dpi=300, bbox_inches='tight')
plt.close()
print("  Guardado: 02_output/figures//outliers_income_boxplot.png")

# ==============================================================================
# PASO 7: CREAR VARIABLES DERIVADAS
# ==============================================================================

print("\n" + "="*80)
print("PASO 7: CREAR VARIABLES DERIVADAS")
print("="*80)

# 1. Log de ingreso
data_filtered['log_income'] = np.log(data_filtered['y_total_m'])
print("\n1. log_income = log(y_total_m)")
print(f"   Creada. Media: {data_filtered['log_income'].mean():.2f}, Std: {data_filtered['log_income'].std():.2f}")

# 2. Edad al cuadrado
data_filtered['age_squared'] = data_filtered['age'] ** 2
print("\n2. age_squared = age²")
print(f"   Creada. Min: {data_filtered['age_squared'].min()}, Max: {data_filtered['age_squared'].max()}")

# 3. Dummy de género
data_filtered['female'] = (data_filtered['sex'] == 0).astype(int)
print("\n3. female = 1 si mujer, 0 si hombre")
print(f"   Creada. Mujeres: {data_filtered['female'].sum()} ({data_filtered['female'].mean()*100:.1f}%)")

# 4. Dummy de formalidad (si no existe)
if 'formal' not in data_filtered.columns and 'cotPension' in data_filtered.columns:
    data_filtered['formal'] = (data_filtered['cotPension'] == 1).astype(int)
    print("\n4. formal = 1 si cotiza pensión, 0 si no")
    print(f"   Creada a partir de cotPension")
    print(f"   Formales: {data_filtered['formal'].sum()} ({data_filtered['formal'].mean()*100:.1f}%)")

print(f"\nVariables derivadas creadas")

# ==============================================================================
# PASO 8: VALIDACIONES FINALES
# ==============================================================================

print("\n" + "="*80)
print("PASO 8: VALIDACIONES FINALES")
print("="*80)

# Checklist
validations = {
    'Sin missings en age': data_filtered['age'].isna().sum() == 0,
    'Sin missings en sex': data_filtered['sex'].isna().sum() == 0,
    'Sin missings en y_total_m': data_filtered['y_total_m'].isna().sum() == 0,
    'log_income sin infinitos': not np.isinf(data_filtered['log_income']).any(),
    'log_income sin NaN': not data_filtered['log_income'].isna().any(),
    'N >= 5000 obs': len(data_filtered) >= 5000,
    'Todas las variables presentes': data_filtered.shape[1] >= vars_original
}

print("\nChecklist de validación:")
for check, passed in validations.items():
    status = "OK" if passed else "FAIL"
    print(f"  [{status}] {check}")

if all(validations.values()):
    print("\nTodas las validaciones pasadas")
else:
    print("\nAlgunas validaciones fallaron - revisar")

# ==============================================================================
# PASO 9: ESTADÍSTICAS DESCRIPTIVAS POR GÉNERO
# ==============================================================================

print("\n" + "="*80)
print("PASO 9: ESTADÍSTICAS DESCRIPTIVAS POR GÉNERO")
print("="*80)

stats_by_gender = data_filtered.groupby('female').agg({
    'y_total_m': ['count', 'mean', 'median', 'std'],
    'age': 'mean',
    'totalHoursWorked': 'mean',
}).round(2)

stats_by_gender.index = ['Hombres', 'Mujeres']
print("\nEstadísticas por género:")
print(stats_by_gender)

# Calcular brecha de ingreso no ajustada
mean_income_male = data_filtered[data_filtered['female']==0]['y_total_m'].mean()
mean_income_female = data_filtered[data_filtered['female']==1]['y_total_m'].mean()
raw_gap = ((mean_income_female - mean_income_male) / mean_income_male) * 100

print(f"\nBrecha de ingreso no ajustada:")
print(f"  Ingreso promedio hombres: ${mean_income_male:,.0f}")
print(f"  Ingreso promedio mujeres: ${mean_income_female:,.0f}")
print(f"  Brecha: {raw_gap:.1f}%")

# ==============================================================================
# PASO 10: GUARDAR DATOS LIMPIOS
# ==============================================================================

print("\n" + "="*80)
print("PASO 10: GUARDAR DATOS LIMPIOS")
print("="*80)

# Guardar dataset limpio
output_path = "00_data/cleaned/data_cleaned.csv"
data_filtered.to_csv(output_path, index=False)

print(f"\nDatos limpios guardados en: {output_path}")
print(f"  Observaciones: {len(data_filtered):,}")
print(f"  Variables: {data_filtered.shape[1]}")

# ==============================================================================
# PASO 11: RESUMEN DE LIMPIEZA
# ==============================================================================

print("\n" + "="*80)
print("PASO 11: RESUMEN DE LIMPIEZA")
print("="*80)

summary_df = pd.DataFrame({
    'Métrica': [
        'Observaciones originales',
        'Observaciones finales',
        'Variables originales',
        'Variables finales',
        'Variables creadas'
    ],
    'Valor': [
        f"{n_original:,}",
        f"{len(data_filtered):,}",
        f"{vars_original}",
        f"{data_filtered.shape[1]}",
        "log_income, age_squared, female, is_outlier_income"
    ]
})

print(summary_df.to_string(index=False))

# Guardar resumen como LaTeX
with open("02_output/tables/cleaning_summary.tex", "w") as f:
    f.write("\\begin{table}[h]\n")
    f.write("\\centering\n")
    f.write("\\caption{Resumen del Proceso de Limpieza de Datos}\n")
    f.write("\\label{tab:cleaning}\n")
    f.write(summary_df.to_latex(index=False, escape=False))
    f.write("\\end{table}\n")

print("\nResumen guardado en: 02_output/tables//cleaning_summary.tex")

# ==============================================================================
# FINALIZACIÓN
# ==============================================================================

print("\n" + "="*80)
print("LIMPIEZA DE DATOS COMPLETADA EXITOSAMENTE")
print("="*80)

print("\nARCHIVOS GENERADOS:")
print("  1. 00_data/cleaned//data_cleaned.csv")
print("  2. 02_output/figures//missing_heatmap.png")
print("  3. 02_output/figures//outliers_income_boxplot.png")
print("  4. 02_output/tables//cleaning_summary.tex")

print("\n" + "="*80)
