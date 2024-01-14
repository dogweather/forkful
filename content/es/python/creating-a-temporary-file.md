---
title:                "Python: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal en tu programa de Python puede ser una herramienta útil para manejar datos temporales o para probar nuevas funciones sin afectar a tus archivos originales.

## Como hacerlo

```Python
# Importar la librería "tempfile"
import tempfile

# Crear un archivo temporal utilizando la función "TemporaryFile"
temp_file = tempfile.TemporaryFile()

# Escribir en el archivo
temp_file.write("¡Hola mundo!")
# Establecer el puntero al principio del archivo
temp_file.seek(0)
# Leer el contenido del archivo
print(temp_file.read())
```

El resultado será: `b'¡Hola mundo!'`

Puedes especificar también el modo de apertura del archivo temporal, por ejemplo, para escribir en él en formato binario:

```Python
# Crear un archivo temporal en modo binario
temp_file_bin = tempfile.TemporaryFile(mode='wb')

# Escribir en el archivo
temp_file_bin.write(b'\x00\xff\x00\xff')
# Establecer el puntero al principio del archivo
temp_file_bin.seek(0)
# Leer el contenido del archivo
print(temp_file_bin.read())
```

El resultado será: `b'\x00\xff\x00\xff'`

## Profundizando

La ventaja de utilizar la librería "tempfile" en lugar de simplemente crear un archivo regular es que el archivo temporal se eliminará automáticamente cuando tu programa finalice su ejecución. Esto es especialmente útil si estás trabajando con grandes cantidades de datos temporales y no quieres ocupar espacio en tu disco duro.

Además, la librería también te permite especificar la ubicación y el prefijo del archivo temporal, así como cambiar su modo de apertura (como vimos en el ejemplo anterior).

Es importante tener en cuenta que los archivos temporales solo existen en la memoria RAM y no tienen una ubicación física en tu disco duro. Por lo tanto, no se pueden abrir o acceder a ellos de forma manual como lo harías con un archivo regular.

## Ver también

- [Documentación oficial de "tempfile" en Python](https://docs.python.org/es/3/library/tempfile.html)
- [Tutorial de Python sobre archivos temporales](https://realpython.com/python-tempfile/)
- [Tutorial de GeeksforGeeks sobre la creación de archivos temporales en Python](https://www.geeksforgeeks.org/temporary-files-python/)

¡Feliz creación de archivos temporales en tu código de Python!