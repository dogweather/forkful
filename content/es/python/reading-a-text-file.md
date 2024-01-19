---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer un archivo de texto en Python significa obtener datos de un archivo txt y procesarlos para su uso. Los programadores lo hacen para trabajar con grandes volúmenes de datos y para guardar y recuperar información.

## Cómo Hacerlo:

Leyendo un archivo de texto es fácil con Python. Aquí te enseño cómo hacerlo en unos pocos pasos.

Primero, abre el archivo. Usa la función open con el nombre de tu archivo:

```Python
archivo = open('mi_archivo.txt', 'r')
```

La 'r' indica que estamos leyendo el archivo. Luego, lee el archivo con read() y asigna el resultado a una variable:

```Python
contenido = archivo.read()
```

Finalmente, asegúrate de cerrar el archivo:

```Python
archivo.close()
```

Como alternativa, puedes usar 'with' para evitar cerrar el archivo:

```Python
with open('mi_archivo.txt', 'r') as archivo:
    contenido = archivo.read()
```

## Inmersión Profunda

En el pasado, herramientas como AWK y Perl dominaban la lectura de archivos. Sin embargo, Python, con su sintaxis sencilla y su facilidad de uso, ha tomado la delantera.

Además del método `read()`, hay otras formas de leer un archivo, como `readline()` y `readlines()`. `readline()` lee el archivo línea por línea, mientras que `readlines()` devuelve todas las líneas como una lista.

Python permite leer archivos en modo binario con 'rb' en lugar de 'r'. Es útil para archivos que no son solo texto.

## Ver También

Para obtener más información sobre este tema, consulta las siguientes referencias:

1. [Documentación de Python sobre la función open](https://docs.python.org/3/library/functions.html#open)
2. [Guía de Python para principiantes sobre la lectura de archivos](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)
3. [Tutorial de Real Python sobre la lectura y escritura de archivos en Python](https://realpython.com/read-write-files-python/)