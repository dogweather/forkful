---
title:                "Python: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Python

Si eres un programador de Python, es muy probable que en algún momento necesites leer un archivo de texto para procesar cierta información. Ya sea para analizar datos, generar reportes o realizar alguna otra tarea, la lectura de archivos de texto es una habilidad que no puede faltar a la hora de programar en este lenguaje.

## Cómo leer un archivo de texto en Python

Para leer un archivo de texto en Python, necesitamos utilizar la función `open()`. Esta función toma dos parámetros: el nombre del archivo y el modo de apertura, que en este caso será de lectura (`"r"`). Además, es buena práctica utilizar el contexto `with` para asegurarse de que el archivo se cierre correctamente después de ser leído.

Este es un ejemplo de cómo leer un archivo de texto en Python y mostrar su contenido:

```python
with open("ejemplo.txt", "r") as archivo:
    contenido = archivo.read()
    print(contenido)
```

El resultado de este código será mostrar todo el contenido del archivo `ejemplo.txt`.

## Profundizando en la lectura de archivos de texto en Python

La función `open()` también nos permite especificar el tipo de codificación que se utilizará al leer el archivo, en caso de que sea necesario. Por defecto, se utiliza la codificación del sistema operativo en que se está ejecutando el código.

Además, existen otras formas de leer archivos de texto en Python, como por ejemplo línea por línea utilizando la función `readline()` o en un bucle utilizando `for line in archivo`. También es importante conocer los métodos de la clase `file` que nos permiten manejar y manipular el archivo de manera más eficiente.

## Ver también

- [Documentación oficial de Python sobre la función `open()`](https://docs.python.org/es/3/library/functions.html#open)
- [Tutorial de W3Schools sobre cómo leer un archivo de texto en Python](https://www.w3schools.com/python/ref_file_read.asp)
- [Explicación detallada de cómo leer archivos de texto en Python](https://towardsdatascience.com/read-write-files-with-python-6b0bba22ddba)