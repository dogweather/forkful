---
title:                "Leyendo un archivo de texto."
html_title:           "Python: Leyendo un archivo de texto."
simple_title:         "Leyendo un archivo de texto."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Leer un archivo de texto es cuando un programa lee el contenido de un archivo de texto. Los programadores hacen esto para poder acceder y manipular la información almacenada en el archivo.

## Cómo hacerlo:
Para leer un archivo de texto en Python, primero debes abrir el archivo en modo lectura utilizando la función open(). Luego, puedes utilizar el método read() para leer su contenido y almacenarlo en una variable.

```Python
archivo = open("ejemplo.txt", "r")

contenido = archivo.read()

print(contenido)
```
**Salida:**
```
Este es un ejemplo de archivo de texto.
Puedes agregar más líneas aquí.
```

## Inmersión profunda:
Leer archivos de texto es una habilidad importante en la programación, ya que muchas veces necesitamos acceder y manipular datos almacenados en archivos. Anteriormente, los lenguajes de programación requerían que se abran y cierren explícitamente los archivos, pero en Python esto se hace automáticamente gracias a la función open(). También existen otros métodos para leer archivos, como readlines() que devuelve una lista con cada línea del archivo.

## Mira también:
Puedes obtener más información sobre la lectura de archivos de texto en Python en la documentación oficial: https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files. También puedes aprender sobre otros métodos de lectura de archivos en Python utilizando el paquete csv o la biblioteca pandas.