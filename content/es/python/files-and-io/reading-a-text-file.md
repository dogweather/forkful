---
date: 2024-01-20 17:55:04.530214-07:00
description: "Leer un archivo de texto en Python es b\xE1sicamente acceder y obtener\
  \ el contenido de un archivo `.txt` en tu c\xF3digo. Los programadores lo hacen\
  \ para\u2026"
lastmod: '2024-03-13T22:44:58.631386-06:00'
model: gpt-4-1106-preview
summary: "Leer un archivo de texto en Python es b\xE1sicamente acceder y obtener el\
  \ contenido de un archivo `.txt` en tu c\xF3digo. Los programadores lo hacen para\u2026"
title: Lectura de un archivo de texto
weight: 22
---

## ¿Qué & Por Qué?

Leer un archivo de texto en Python es básicamente acceder y obtener el contenido de un archivo `.txt` en tu código. Los programadores lo hacen para trabajar con datos, configuraciones, análisis o simplemente para importar y exportar información.

## Cómo:

Leer un archivo es simple. Usa `open()` con un `with` statement para manejar el archivo de manera segura. Aquí tienes un código de ejemplo y el resultado:

```Python
# Leer todo el contenido de un archivo
with open('ejemplo.txt', 'r') as archivo:
    contenido = archivo.read()
print(contenido)

# Leer línea por línea y almacenar en una lista
with open('ejemplo.txt', 'r') as archivo:
    lineas = archivo.readlines()
print(lineas)

# Leer línea por línea con un loop
with open('ejemplo.txt', 'r') as archivo:
    for linea in archivo:
        print(linea, end='')
```

Si `ejemplo.txt` tiene el texto "Hola Mundo\nPython es genial\n", la salida será:

```
Hola Mundo
Python es genial

['Hola Mundo\n', 'Python es genial\n']

Hola Mundo
Python es genial
```

## Profundización:

Leer archivos de texto es un fundamento de la programación que existe desde los inicios de los ordenadores. Información como registros, configuraciones y scripts son comúnmente almacenados y leídos como texto plano.

Antes de Python, los lenguajes como C requerían manejar manualmente la memoria y los punteros de archivos, pero Python simplifica estas operaciones con su manejo automático de recursos.

Existen alternativas a `open()`, como `fileinput` para procesamiento eficiente de líneas y módulos como `io` que permiten trabajar con datos en memoria como si fueran archivos. 

En cuanto a detalles de implementación, abrir un archivo con `open()` en Python crea un objeto de archivo que tiene métodos como `.read()`, `.readline()`, y `.readlines()` para leer contenido en diferentes formas.

## Ver También:

- Documentación oficial de Python sobre E/S de archivos: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Tutorial de archivos en Python en Real Python: https://realpython.com/read-write-files-python/
- Una guía detallada sobre el manejo de archivos en Programiz: https://www.programiz.com/python-programming/file-operation
