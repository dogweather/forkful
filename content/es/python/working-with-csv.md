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

## ¿Por qué trabajar con CSV?

CSV (Comma Separated Values) es un formato comúnmente utilizado para almacenar datos de manera tabular, lo que lo hace ideal para almacenar grandes cantidades de información en un formato fácil de entender y compartir con otros. Al trabajar con CSV en Python, puede analizar y manipular datos de manera eficiente y precisa, lo que lo convierte en una herramienta esencial para cualquier programador.

## Cómo hacerlo

Para trabajar con CSV en Python, necesitarás importar el módulo `csv` en tu programa. Luego, puedes usar la función `reader()` para leer un archivo CSV y convertirlo en un objeto que puedas manipular. Aquí hay un ejemplo de cómo puedes leer un archivo CSV y mostrar sus primeras cinco filas:

```Python
import csv

with open("datos.csv") as archivo:
    csv_reader = csv.reader(archivo, delimiter=",")
    for row in csv_reader:
        print(row[:5])
```

Este código abrirá el archivo `datos.csv`, lo leerá y lo imprimirá en la consola. Puedes especificar el delimitador en caso de que tu archivo CSV utilice un carácter diferente al de la coma.

También puedes escribir datos en un archivo CSV utilizando la función `writer()`. Aquí hay un ejemplo de cómo puedes escribir una lista de nombres en un archivo CSV:

```Python
import csv

nombres = ["Pedro", "María", "Juan", "Ana", "Diego"]

with open("nombres.csv", "w") as archivo:
    csv_writer = csv.writer(archivo)
    for nombre in nombres:
        csv_writer.writerow([nombre])
```

Esto creará un archivo `nombres.csv` con los nombres en una sola columna. Puedes experimentar con diferentes opciones de delimitador y formatos de archivo para adaptarlo a tus necesidades específicas.

## Profundizando en el mundo de CSV

Aunque CSV es un formato simple y fácil de usar, también tiene sus complicaciones. Por ejemplo, si tus datos contienen comillas o caracteres especiales, puede haber problemas al leer o escribir un archivo CSV. Afortunadamente, el módulo `csv` de Python tiene funciones para manejar estas situaciones y asegurar que tus datos se manejen correctamente.

Otra cosa a tener en cuenta al trabajar con CSV en Python es la eficiencia. Si estás trabajando con grandes conjuntos de datos, puede ser más rápido y menos exigente para la memoria utilizar el módulo `pandas` en lugar de `csv`. Este módulo ofrece más funciones y opciones avanzadas para trabajar con CSV, pero también puede ser más complejo para los principiantes.

## Ver también

- [Documentación del módulo csv de Python](https://docs.python.org/es/3/library/csv.html)
- [Tutorial de DataCamp sobre trabajar con CSV en Python](https://www.datacamp.com/community/tutorials/python-read-csv)
- [Documentación de Pandas para trabajar con CSV](https://pandas.pydata.org/pandas-docs/stable/user_guide/io.html#csv-text-files)