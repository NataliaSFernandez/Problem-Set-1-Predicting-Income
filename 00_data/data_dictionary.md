# Diccionario de Variables - Problem Set 1

## GEIH 2018 Bogotá - Variables Utilizadas

Este diccionario describe las variables utilizadas en el Problem Set 1. Se basa en el diccionario oficial (https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html) proporcionado por el profesor y en la documentación del DANE (https://ignaciomsarmiento.github.io/GEIH2018_sample/ddi-documentation-spanish-608.pdf).

---

## VARIABLE OUTCOME (Dependiente)

### y_total_m
**Definición:** Ingreso laboral total mensual (asalariados + independientes) - nominal  
**Tipo:** Numérica continua  
**Unidad:** Pesos colombianos (COP)  
**Construcción:** Suma de ingresos monetarios de todas las ocupaciones  
**Uso en PS1:** Variable dependiente principal en todas las regresiones  
**Notas:**
- Incluye salario base + horas extras + bonificaciones + comisiones
- Incluye ingresos de cuenta propia
- NO incluye ingresos en especie (se registran por separado)

---

## VARIABLES DEMOGRÁFICAS

### age
**Definición:** Edad en años cumplidos  
**Tipo:** Numérica continua  
**Rango:** 0 - 106 años (en dataset original); 18+ en análisis  
**Uso en PS1:** Variable explicativa principal (teoría capital humano)  
**Transformaciones:**
- age_squared: edad al cuadrado para capturar concavidad en perfil edad-ingreso

### sex
**Definición:** Sexo del individuo  
**Tipo:** Categórica binaria  
**Valores:**
- 0 = Mujer
- 1 = Hombre  
**Uso en PS1:** Variable clave para análisis de brecha de género (Sección 2)  
**Transformaciones:**
- female: Dummy donde 1=Mujer, 0=Hombre

---

## VARIABLES DE CAPITAL HUMANO

### maxEducLevel
**Definición:** Máximo nivel educativo alcanzado  
**Tipo:** Categórica ordinal  
**Valores:** 
- 1 = Ninguno
- 2 = Preescolar
- 3 = Primaria
- 4 = Secundaria
- 5 = Media
- 6 = Superior/Universitaria
- 7 = Posgrado  
**Uso en PS1:** Control de capital humano pre-mercado  
**Justificación:** Proxy de habilidades y productividad observada

---

## VARIABLES DE EMPLEO Y OCUPACIÓN

### relab
**Definición:** Tipo de relación laboral / ocupación  
**Tipo:** Categórica  
**Valores:**
- 1 = Obrero/empleado empresa particular
- 2 = Obrero/empleado gobierno
- 3 = Empleado doméstico
- 4 = Trabajador por cuenta propia
- 5 = Patrón o empleador
- 6 = Trabajador familiar sin remuneración
- 7 = Trabajador sin remuneración en empresas
- 8 = Jornalero o peón
- 9 = Otro  
**Uso en PS1:** Control especificado explícitamente en Problem Set (Sección 1)  
**Justificación:** Captura heterogeneidad en tipo de contrato

### oficio
**Definición:** Ocupación según clasificación CIUO  
**Tipo:** Categórica (código de ocupación)  
**Valores:** Códigos de 1-99 (clasificación CIUO-08)  
**Uso en PS1:** Efectos fijos de ocupación  
**Justificación:** Aproxima comparaciones de igual trabajo

### formal
**Definición:** Indicador de formalidad laboral  
**Tipo:** Binaria (0/1)  
**Construcción:**
- 1 = Cotiza a seguridad social (pensiones)
- 0 = No cotiza  
**Variable origen:** Derivada de cotPension (p6920)  
**Uso en PS1:** Control de formalidad/cobertura de seguridad social  
**Justificación:** Dimensión clave de segmentación del mercado laboral

### sizeFirm
**Definición:** Tamaño de la firma por categorías  
**Tipo:** Categórica ordinal  
**Variable origen:** Creada a partir de p6870 (número de empleados)  
**Categorías típicas:**
- Microempresa: 1-10 empleados
- Pequeña: 11-50
- Mediana: 51-200
- Grande: 200+  
**Uso en PS1:** Control de heterogeneidad de firmas  
**Justificación:** Premia salarial por tamaño de firma

### p6240
**Definición:** Actividad económica / sector en el que trabajó la semana pasada  
**Tipo:** Categórica (código CIIU)  
**Uso en PS1:** Efectos fijos de industria/sector  
**Justificación:** Premia salarial sectorial

### p6426
**Definición:** Tiempo (en meses) que lleva trabajando en la empresa/negocio actual  
**Tipo:** Numérica continua  
**Unidad:** Meses  
**Uso en PS1:** Tenure (antigüedad en la firma)  
**Justificación:** Experiencia específica dentro de la firma

---

## VARIABLES DE OFERTA LABORAL

### totalHoursWorked
**Definición:** Total de horas trabajadas la semana anterior (todas las ocupaciones)  
**Tipo:** Numérica continua  
**Unidad:** Horas por semana  
**Uso en PS1:** Control especificado explícitamente en Problem Set (Sección 1)  
**Justificación:** 
- Intensidad de oferta laboral
- Diferencia entre trabajadores part-time vs full-time

---

## VARIABLES DE CONTEXTO TEMPORAL

### mes
**Definición:** Mes de la entrevista  
**Tipo:** Categórica (1-12)  
**Valores:** 1=Enero, 2=Febrero, ..., 12=Diciembre  
**Uso en PS1:** Control de estacionalidad  
**Justificación:** Controlar variaciones temporales en empleo/ingresos

---

## VARIABLES AUXILIARES (Para Filtros)

### ocu
**Definición:** Persona ocupada (empleada)  
**Tipo:** Binaria (0/1)  
**Valores:**
- 1 = Ocupado
- 0 = No ocupado  
**Uso en PS1:** Filtro para seleccionar muestra (empleados solamente)

---

## VARIABLES DERIVADAS (Creadas en Limpieza)

### log_income
**Definición:** Logaritmo natural del ingreso laboral mensual  
**Tipo:** Numérica continua  
**Construcción:** log(y_total_m)  
**Uso en PS1:** Variable dependiente en regresiones log-lineales  
**Justificación:**
- Normaliza distribución sesgada del ingreso
- Interpretación como elasticidad / cambio porcentual
- Reduce influencia de outliers

### age_squared
**Definición:** Edad al cuadrado  
**Tipo:** Numérica continua  
**Construcción:** edad al cuadrado  
**Uso en PS1:** Capturar no-linealidad en perfil edad-ingreso  
**Justificación:** 
- Teoría de capital humano predice relación cóncava
- Permite estimar edad de peak earnings

### female
**Definición:** Indicador de género femenino  
**Tipo:** Binaria (0/1)  
**Construcción:** 1 if sex==0, else 0  
**Valores:**
- 1 = Mujer
- 0 = Hombre  
**Uso en PS1:** Variable de interés en análisis de brecha de género  
**Justificación:** Codificación más intuitiva que sex

### is_outlier_income
**Definición:** Indicador de outlier en ingreso (método IQR)  
**Tipo:** Binaria (0/1)  
**Construcción:** 
- 1 si y_total_m < Q1 - 1.5*IQR o y_total_m > Q3 + 1.5*IQR
- 0 en caso contrario  
**Uso en PS1:** Análisis de robustez, identificar observaciones influyentes  
**Nota:** Outliers se mantienen en el análisis

---

## NOTAS METODOLÓGICAS

### Decisiones de Limpieza Aplicadas:

1. **Filtros de observaciones:**
   - age >= 18: Mayores de edad
   - ocu == 1: Empleados
   - y_total_m > 0: Ingresos positivos

2. **Tratamiento de missings:**
   - sex: Observaciones eliminadas (variable crítica)
   - totalHoursWorked: Imputación con mediana por sexo
   - Variables categóricas (relab, oficio, etc.): Imputación con moda
   - p6426 (tenure): Imputación con mediana

3. **Tratamiento de outliers:**
   - NO se eliminan
   - Se usa transformación logarítmica en regresiones
   - Se crea flag is_outlier_income para análisis

4. **Variables mantenidas:**
   - Todas las 178 variables originales se mantienen en el dataset

---

## REFERENCIAS

- Diccionario oficial: https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html
- Documentación DANE: https://ignaciomsarmiento.github.io/GEIH2018_sample/ddi-documentation-spanish-608.pdf
- Códigos y etiquetas: https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html
- Problem Set 1: PDF del taller

---


