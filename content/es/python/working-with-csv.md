---
title:                "Trabajando con archivos csv"
html_title:           "Python: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con CSV es una forma de manejar y almacenar datos tabulares en formato de texto plano. Los programadores utilizan esta técnica porque es fácil de leer y escribir, y puede ser utilizado en una amplia variedad de aplicaciones.

## Cómo:

```Python
# Importar el módulo csv
import csv

# Crear un archivo CSV
with open('datos.csv', 'w') as archivo:
    # Crear un escritor de CSV
    escritor = csv.writer(archivo)
    # Escribir una fila de datos
    escritor.writerow(['Nombre', 'Edad', 'País'])
    # Escribir otra fila de datos
    escritor.writerow(['Ana', '27', 'España'])

# Leer un archivo CSV
with open('datos.csv') as archivo:
    # Crear un lector de CSV
    lector = csv.reader(archivo)
    # Recorrer todas las filas del archivo
    for fila in lector:
        # Imprimir la fila
        print(fila)

# Salida:
# ['Nombre', 'Edad', 'País']
# ['Ana', '27', 'España']
```

## Profundizando:

### Contexto Histórico:

CSV significa Comma Separated Values y fue creado originalmente en los años 70 para almacenar datos en hojas de cálculo. Con el tiempo, se convirtió en un formato de archivo ampliamente utilizado para intercambiar datos tabulares entre diferentes sistemas.

### Alternativas:

Algunas alternativas a CSV incluyen formatos tabulares más avanzados como JSON o XML. Sin embargo, CSV sigue siendo popular debido a su simplicidad y capacidad de ser abierto en cualquier editor de texto.

### Detalles de Implementación:

El módulo csv de Python proporciona funciones para leer y escribir archivos CSV. Asegúrate de especificar el modo adecuado (w para escritura, r para lectura) y el delimitador (coma por defecto) al abrir un archivo CSV.

## Ver también:

- [Página oficial del módulo csv de Python](https://docs.python.org/3/library/csv.html)
- [Guía completa de trabajo con CSV en Python](https://realpython.com/python-csv/)