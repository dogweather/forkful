---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con archivos CSV implica manejar archivos de texto que contienen datos separados por comas. Los programadores los usan por su simplicidad y compatibilidad universal para el intercambio de datos.

## Cómo hacerlo:

Para leer un CSV en Python:

```Python
import csv

nombre_archivo = 'ejemplo.csv'

with open(nombre_archivo, mode='r', encoding='utf-8') as archivo:
    lector_csv = csv.reader(archivo)
    for fila in lector_csv:
        print(fila)
```

Para escribir en un CSV en Python:

```Python
import csv

nombre_archivo = 'ejemplo.csv'
datos = [['nombre', 'pais', 'email'], ['Juan', 'España', 'juan@example.com']]

with open(nombre_archivo, mode='w', newline='', encoding='utf-8') as archivo:
    escritor_csv = csv.writer(archivo)
    escritor_csv.writerows(datos)
```

Salida de ejemplo al leer `ejemplo.csv`:

```Python
['nombre', 'pais', 'email']
['Juan', 'España', 'juan@example.com']
```

## Análisis Profundo

El formato CSV (Valores Separados por Comas) tiene su origen en los primeros días de la informática personal. A pesar de su antigüedad, sigue siendo relevante por su simplicidad y facilidad de uso con hojas de cálculo. Alternativas modernas incluyen JSON y XML, pero estos son más complejos. Cuando se trabaja con CSV en Python, es importante considerar el manejo de caracteres especiales, como comas en los datos y problemas de codificación, lo que puede requerir un procesamiento más cuidadoso.

## Ver También

- Documentación oficial Python CSV: https://docs.python.org/3/library/csv.html
- Guía de pandas para manejo de CSV: https://pandas.pydata.org/pandas-docs/stable/user_guide/io.html#csv-text-files
- Tutorial de w3schools para leer/escribir archivos CSV en Python: https://www.w3schools.com/python/python_csv.asp
