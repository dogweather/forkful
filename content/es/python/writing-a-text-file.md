---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto en Python significa almacenar datos en un formato legible. Los programadores lo hacen para guardar configuraciones, resultados de programas, o compartir información entre distintos procesos.

## Cómo hacerlo:
Crear y escribir en un archivo es sencillo:

```python
# Abrir un archivo para escritura
with open('ejemplo.txt', 'w') as archivo:
    archivo.write("Hola, mundo!")

# Verificar el contenido
with open('ejemplo.txt', 'r') as archivo:
    print(archivo.read())
```

Salida:

```
Hola, mundo!
```

Para añadir texto al final de un archivo existente, utiliza el modo 'a':

```python
# Añadir más contenido al archivo existente
with open('ejemplo.txt', 'a') as archivo:
    archivo.write("\nAdiós, mundo!")

# Verificar el contenido actualizado
with open('ejemplo.txt', 'r') as archivo:
    print(archivo.read())
```

Salida:

```
Hola, mundo!
Adiós, mundo!
```

## Profundizando:
Históricamente, escribir en archivos de texto ha sido una de las primeras formas de persistencia de datos en computación. Alternativas modernas incluyen bases de datos o almacenamiento en la nube, pero los archivos de texto permanecen populares por su simplicidad. A nivel de implementación, Python maneja la escritura de archivos utilizando las funciones incorporadas `open()`, `write()` y `close()`, manejados automáticamente con el contexto `with` para garantizar el cierre del archivo.

## Véase También:
Para más información sobre manejo de archivos en Python, visita:
- Documentación oficial de Python sobre E/S de archivos: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Tutorial en Real Python: https://realpython.com/read-write-files-python/
- W3Schools - Python File Handling: https://www.w3schools.com/python/python_file_handling.asp
