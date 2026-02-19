"""
==============================================================================
PROBLEM SET 1: PREDICTING INCOME
Script 03: Descriptive Statistics
==============================================================================
OBJETIVO: Generar estadísticas descriptivas de datos limpios

INPUTS:  00_data/cleaned/data_cleaned.csv
OUTPUTS: 
  - 02_output/tables/descriptive_stats_general.tex
  - 02_output/tables/descriptive_stats_gender.tex
  - 02_output/figures/income_distribution_gender.png
  - 02_output/figures/age_distribution.png
  - 02_output/figures/hours_distribution_gender.png
==============================================================================
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

# Configuración de visualización
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")

# Crear directorios si no existen
os.makedirs("02_output/figures", exist_ok=True)
os.makedirs("02_output/tables", exist_ok=True)

print("="*80)
print("ESTADÍSTICAS DESCRIPTIVAS - PROBLEM SET 1")
print("="*80)

# ==============================================================================
# PASO 1: CARGAR DATOS LIMPIOS
# ==============================================================================

print("\n" + "="*80)
print("PASO 1: CARGAR DATOS LIMPIOS")
print("="*80)

data = pd.read_csv("00_data/cleaned/data_cleaned.csv")

print(f"\nDatos cargados exitosamente")
print(f"  Observaciones: {len(data):,}")
print(f"  Variables: {data.shape[1]}")

# ==============================================================================
# PASO 2: ESTADÍSTICAS GENERALES DE LA MUESTRA
# ==============================================================================

print("\n" + "="*80)
print("PASO 2: ESTADÍSTICAS GENERALES")
print("="*80)

# Variables continuas clave
vars_continuas = {
    'y_total_m': 'Ingreso mensual (COP)',
    'log_income': 'Log(Ingreso)',
    'age': 'Edad (años)',
    'totalHoursWorked': 'Horas trabajadas/semana'
}

# Calcular estadísticas
stats_general = pd.DataFrame()
for var, label in vars_continuas.items():
    stats_general[label] = [
        data[var].count(),
        data[var].mean(),
        data[var].std(),
        data[var].min(),
        data[var].quantile(0.25),
        data[var].median(),
        data[var].quantile(0.75),
        data[var].max()
    ]

stats_general.index = ['N', 'Media', 'Desv. Est.', 'Mínimo', 'P25', 'Mediana', 'P75', 'Máximo']

print("\nEstadísticas generales:")
print(stats_general.to_string())

# Guardar en LaTeX
with open("02_output/tables/003_descriptive_stats/descriptive_stats_general.tex", "w") as f:
    f.write("\\begin{table}[h]\n")
    f.write("\\centering\n")
    f.write("\\caption{Estadísticas Descriptivas Generales}\n")
    f.write("\\label{tab:desc_general}\n")
    f.write(stats_general.to_latex(float_format="%.2f", escape=False))
    f.write("\\end{table}\n")

print("\nGuardado: 02_output/tables/003_descriptive_stats/descriptive_stats_general.tex")

# ==============================================================================
# PASO 3: ESTADÍSTICAS POR GÉNERO
# ==============================================================================

print("\n" + "="*80)
print("PASO 3: ESTADÍSTICAS POR GÉNERO")
print("="*80)

# Calcular estadísticas por género
stats_gender = data.groupby('female').agg({
    'y_total_m': ['count', 'mean', 'median', 'std'],
    'log_income': ['mean', 'std'],
    'age': 'mean',
    'totalHoursWorked': 'mean'
}).round(2)

# Renombrar índices
stats_gender.index = ['Hombres', 'Mujeres']

print("\nEstadísticas por género:")
print(stats_gender)

# Calcular brecha de ingreso
mean_male = data[data['female']==0]['y_total_m'].mean()
mean_female = data[data['female']==1]['y_total_m'].mean()
gap_pct = ((mean_female - mean_male) / mean_male) * 100

median_male = data[data['female']==0]['y_total_m'].median()
median_female = data[data['female']==1]['y_total_m'].median()
gap_median_pct = ((median_female - median_male) / median_male) * 100

print(f"\nBrecha de ingreso (no ajustada):")
print(f"  Media - Hombres: ${mean_male:,.0f}")
print(f"  Media - Mujeres: ${mean_female:,.0f}")
print(f"  Brecha (media): {gap_pct:.1f}%")
print(f"  Mediana - Hombres: ${median_male:,.0f}")
print(f"  Mediana - Mujeres: ${median_female:,.0f}")
print(f"  Brecha (mediana): {gap_median_pct:.1f}%")

# Guardar tabla en LaTeX
with open("02_output/tables/003_descriptive_stats/descriptive_stats_gender.tex", "w") as f:
    f.write("\\begin{table}[h]\n")
    f.write("\\centering\n")
    f.write("\\caption{Estadísticas Descriptivas por Género}\n")
    f.write("\\label{tab:desc_gender}\n")
    f.write(stats_gender.to_latex(float_format="%.2f", escape=False))
    f.write("\\end{table}\n")

print("\nGuardado: 02_output/tables/003_descriptive_stats   /descriptive_stats_gender.tex")

# ==============================================================================
# PASO 4: COMPOSICIÓN DE LA MUESTRA
# ==============================================================================

print("\n" + "="*80)
print("PASO 4: COMPOSICIÓN DE LA MUESTRA")
print("="*80)

# Por género
gender_comp = data['female'].value_counts().sort_index()
print(f"\nDistribución por género:")
print(f"  Hombres: {gender_comp[0]:,} ({gender_comp[0]/len(data)*100:.1f}%)")
print(f"  Mujeres: {gender_comp[1]:,} ({gender_comp[1]/len(data)*100:.1f}%)")

# Por nivel educativo
print("\nDistribución por nivel educativo:")
if 'maxEducLevel' in data.columns:
    edu_dist = data['maxEducLevel'].value_counts().sort_index()
    for level, count in edu_dist.items():
        pct = (count / len(data)) * 100
        print(f"  Nivel {level}: {count:,} ({pct:.1f}%)")

# Por tipo de contrato
print("\nDistribución por tipo de relación laboral:")
if 'relab' in data.columns:
    relab_dist = data['relab'].value_counts().sort_index()
    for rel, count in relab_dist.head(5).items():
        pct = (count / len(data)) * 100
        print(f"  Tipo {rel}: {count:,} ({pct:.1f}%)")

# Por formalidad
print("\nDistribución por formalidad:")
if 'formal' in data.columns:
    formal_dist = data['formal'].value_counts()
    for status, count in formal_dist.items():
        pct = (count / len(data)) * 100
        label = "Formal" if status == 1 else "Informal"
        print(f"  {label}: {count:,} ({pct:.1f}%)")

# ==============================================================================
# PASO 5: GRÁFICOS DE DISTRIBUCIONES
# ==============================================================================

print("\n" + "="*80)
print("PASO 5: GENERAR GRÁFICOS")
print("="*80)

# GRÁFICO 1: Distribución de ingresos por género
print("\nGenerando: Distribución de ingresos por género...")
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# Histogramas separados
data_male = data[data['female']==0]['y_total_m']
data_female = data[data['female']==1]['y_total_m']

axes[0].hist(data_male, bins=50, alpha=0.7, label='Hombres', edgecolor='black')
axes[0].axvline(data_male.mean(), color='blue', linestyle='--', linewidth=2, label=f'Media: ${data_male.mean():,.0f}')
axes[0].set_xlabel('Ingreso Mensual (COP)', fontsize=12)
axes[0].set_ylabel('Frecuencia', fontsize=12)
axes[0].set_title('Distribución Ingreso - Hombres', fontsize=14, fontweight='bold')
axes[0].legend()
axes[0].grid(True, alpha=0.3)

axes[1].hist(data_female, bins=50, alpha=0.7, label='Mujeres', color='orange', edgecolor='black')
axes[1].axvline(data_female.mean(), color='red', linestyle='--', linewidth=2, label=f'Media: ${data_female.mean():,.0f}')
axes[1].set_xlabel('Ingreso Mensual (COP)', fontsize=12)
axes[1].set_ylabel('Frecuencia', fontsize=12)
axes[1].set_title('Distribución Ingreso - Mujeres', fontsize=14, fontweight='bold')
axes[1].legend()
axes[1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig("02_output/figures/003_descriptive_stats/income_distribution_gender.png", dpi=300, bbox_inches='tight')
plt.close()
print("  Guardado: 02_output/figures/003_descriptive_stats/income_distribution_gender.png")

# GRÁFICO 2: Distribución de edad
print("\nGenerando: Distribución de edad...")
fig, ax = plt.subplots(figsize=(10, 6))

ax.hist(data['age'], bins=30, edgecolor='black', alpha=0.7)
ax.axvline(data['age'].mean(), color='red', linestyle='--', linewidth=2, label=f'Media: {data["age"].mean():.1f} años')
ax.axvline(data['age'].median(), color='green', linestyle='--', linewidth=2, label=f'Mediana: {data["age"].median():.1f} años')
ax.set_xlabel('Edad (años)', fontsize=12)
ax.set_ylabel('Frecuencia', fontsize=12)
ax.set_title('Distribución de Edad en la Muestra', fontsize=14, fontweight='bold')
ax.legend()
ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig("02_output/figures/003_descriptive_stats/age_distribution.png", dpi=300, bbox_inches='tight')
plt.close()
print("  Guardado: 02_output/figures/003_descriptive_stats/age_distribution.png")

# GRÁFICO 3: Horas trabajadas por género
print("\nGenerando: Horas trabajadas por género...")
fig, ax = plt.subplots(figsize=(10, 6))

data_hours = data[['female', 'totalHoursWorked']].copy()
data_hours['Género'] = data_hours['female'].map({0: 'Hombres', 1: 'Mujeres'})

sns.boxplot(data=data_hours, x='Género', y='totalHoursWorked', ax=ax)
ax.set_ylabel('Horas Trabajadas por Semana', fontsize=12)
ax.set_xlabel('Género', fontsize=12)
ax.set_title('Distribución de Horas Trabajadas por Género', fontsize=14, fontweight='bold')
ax.grid(True, alpha=0.3, axis='y')

# Agregar medias
for i, gender in enumerate([0, 1]):
    mean_val = data[data['female']==gender]['totalHoursWorked'].mean()
    ax.text(i, mean_val, f'Media: {mean_val:.1f}', 
            horizontalalignment='center', fontsize=10, color='red', fontweight='bold')

plt.tight_layout()
plt.savefig("02_output/figures/003_descriptive_stats/hours_distribution_gender.png", dpi=300, bbox_inches='tight')
plt.close()
print("  Guardado: 02_output/figures/003_descriptive_stats/hours_distribution_gender.png")

# ==============================================================================
# PASO 6: RESUMEN FINAL
# ==============================================================================

print("\n" + "="*80)
print("ESTADÍSTICAS DESCRIPTIVAS COMPLETADAS")
print("="*80)

print("\nARCHIVOS GENERADOS:")
print("\nTablas:")
print("  1. 02_output/tables/003_descriptive_stats/descriptive_stats_general.tex")
print("  2. 02_output/tables/003_descriptive_stats/descriptive_stats_gender.tex")
print("\nGráficos:")
print("  1. 02_output/figures/003_descriptive_stats/income_distribution_gender.png")
print("  2. 02_output/figures/003_descriptive_stats/age_distribution.png")
print("  3. 02_output/figures/003_descriptive_stats/hours_distribution_gender.png")

print("\nRESUMEN CLAVE:")
print(f"  Muestra final: {len(data):,} empleados")
print(f"  Hombres: {gender_comp[0]:,} ({gender_comp[0]/len(data)*100:.1f}%)")
print(f"  Mujeres: {gender_comp[1]:,} ({gender_comp[1]/len(data)*100:.1f}%)")
print(f"  Edad promedio: {data['age'].mean():.1f} años")
print(f"  Ingreso promedio: ${data['y_total_m'].mean():,.0f}")
print(f"  Brecha de género (media): {gap_pct:.1f}%")

print("\n" + "="*80)
