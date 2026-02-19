"""
Script: 01_Data_scrapper.py

Descripción:
Este script descarga y consolida los datos de la Encuesta GEIH 2018 disponibles
en formato HTML en el repositorio público
https://ignaciomsarmiento.github.io/GEIH2018_sample/.

El script realiza solicitudes HTTP a múltiples páginas (chunks) de datos,
extrae la tabla contenida en cada página, y une toda la información en un único
DataFrame de pandas.

Entradas:
- Páginas HTML públicas (geih_page_1.html a geih_page_10.html).

Salidas:
- Un archivo CSV llamado 'geih_2018_completo.csv' que contiene la unión de todas
  las tablas extraídas.
- Impresiones en consola del estado de cada solicitud HTTP y una vista previa
  de los datos consolidados.

Dependencias:
- requests
- pandas
- beautifulsoup4
"""

import requests
import pandas as pd
from bs4 import BeautifulSoup

dfs = []  

for num in range(1, 11):  # 1 a 11
    url = f"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_{num}.html"
    headers = {"User-Agent": "Mozilla/5.0"}

    r = requests.get(url, headers=headers)
    print(f"Página {num}: {r.status_code}")
    r.raise_for_status()

    soup = BeautifulSoup(r.text, "html.parser")
    table = soup.find("table")

    # headers
    cols = [th.get_text(strip=True) for th in table.select("thead th")]
    if cols and cols[0] == "":
        cols[0] = "row_id"

    # filas
    rows = []
    for tr in table.select("tbody tr"):
        cells = [td.get_text(strip=True) for td in tr.select("td")]
        rows.append(cells)

    df_page = pd.DataFrame(rows, columns=cols)
    df_page.insert(0, "chunk", num)  # Agregar columna 'chunk' al inicio
    dfs.append(df_page)

# unir todo
df_final = pd.concat(dfs, ignore_index=True)

print(df_final.head())
print("Filas:", df_final.shape[0], "Columnas:", df_final.shape[1])

df_final.to_csv("00_data/raw/geih_2018_completo.csv", index=False, encoding="utf-8")
print("Guardado geih_2018_completo.csv")



