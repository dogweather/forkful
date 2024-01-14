---
title:                "Python: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con archivos CSV en Python?

El formato CSV (valores separados por comas) es una forma conveniente de almacenar y organizar datos en una estructura tabular. En Python, trabajar con archivos CSV es una habilidad valiosa ya que permite la manipulación y análisis de datos de manera eficiente. ¡Sigue leyendo para aprender cómo trabajar con archivos CSV en Python!

## Cómo hacerlo

Para comenzar a trabajar con archivos CSV en Python, primero debes importar el módulo `csv`. Luego, puedes utilizar la función `reader()` para leer un archivo CSV y convertirlo en un objeto que pueda ser iterado.

```
import csv # Importa el módulo csv
with open('datos.csv', 'r') as archivo:
  lector = csv.reader(archivo)
  for fila in lector:
    print(fila)
```

Este código lee el archivo `datos.csv` y lo imprime en la consola en formato de lista. Cada fila se muestra como una lista, con cada elemento separado por una coma. También puedes acceder a elementos específicos utilizando la indexación de lista.

```
# Continuando con el ejemplo anterior
print(fila[0]) # Imprime el primer elemento de la fila
```

El módulo `csv` también proporciona una función `writer()` para escribir datos en un archivo CSV. Primero, necesitas abrir un archivo en modo de escritura y luego crear un objeto escritor utilizando la función `writer()`. Luego, puedes utilizar el método `writerow()` para escribir una fila en el archivo.

```
# Abre un archivo llamado "datos_salida.csv" en modo de escritura
with open('datos_salida.csv', 'w') as archivo:
  escritor = csv.writer(archivo)
  lista = ['Manzana', 'Banana', 'Naranja']
  escritor.writerow(lista) # Escribe la lista en una fila en el archivo
```

El archivo `datos_salida.csv` contendrá una fila con las palabras "Manzana", "Banana" y "Naranja", separadas por comas.

## Profundizando

Además de las funciones básicas de lectura y escritura, el módulo `csv` también ofrece opciones avanzadas para trabajar con archivos CSV. Por ejemplo, puedes utilizar el argumento `delimiter` para definir un delimitador personalizado en lugar de una coma. También puedes utilizar el argumento `quotechar` para definir un carácter de cita diferente al normalmente utilizado.

Si necesitas trabajar con archivos CSV que contienen valores numéricos, puedes utilizar el módulo `decimal` para evitar la pérdida de precisión al convertir los números a formato de coma flotante.

## Ver también

- [Documentación oficial del módulo csv en Python](https://docs.python.org/es/3/library/csv.html)
- [Tutorial de Real Python sobre trabajar con archivos CSV en Python](https://realpython.com/python-csv/)
- [Guía de Dataquest sobre procesamiento de datos con archivos CSV en Python](https://www.dataquest.io/blog/python-csv/)

¡Ahora estás listo para trabajar con archivos CSV en Python! ¡Explora diferentes opciones y comienza a jugar con tus propios datos para descubrir el poder que ofrece este formato de archivo!