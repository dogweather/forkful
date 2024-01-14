---
title:                "Python: Comprobando si existe un directorio"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías comprobar si un directorio existe?

A veces, al escribir un programa en Python, es posible que necesites saber si un directorio específico existe en tu sistema de archivos. Esto puede ser útil para evitar errores o para realizar acciones específicas en función de si el directorio está presente o no. En esta publicación, exploraremos cómo podemos verificar la existencia de un directorio en Python.

## Cómo comprobar si un directorio existe en Python

Para comprobar si un directorio existe en Python, podemos utilizar la función `path.exists()` de la librería estándar `os`. Esta función acepta una ruta como argumento y devuelve `True` si existe un archivo o directorio en esa ruta, o `False` si no existe.

Veamos un ejemplo de cómo podemos utilizar esta función:

```Python
import os

ruta = "/Users/usuario/MiProyecto/"

if os.path.exists(ruta):
    print("El directorio existe en la ruta especificada.")
else:
    print("El directorio no existe en la ruta especificada.")
```

En este ejemplo, primero importamos la librería `os` para poder utilizar la función `path.exists()`. Luego, definimos una variable `ruta` que contiene la ruta del directorio que queremos comprobar. Luego, utilizamos un condicional `if` para verificar si el directorio existe o no. El resultado se imprimirá en la consola.

Si queremos verificar la existencia de un directorio en un sistema de archivos diferente, podemos proporcionar una ruta absoluta o usar la función `path.join()` de `os` para unir varios componentes de la ruta.

## Profundizando en la comprobación de la existencia de directorios

En algunos casos, puede ser necesario realizar una verificación más específica sobre un directorio. Por ejemplo, podemos querer saber si el directorio es realmente un directorio y no un archivo con el mismo nombre. En este caso, podemos utilizar la función `os.path.isdir()` que nos devolverá `True` si el camino dado es un directorio válido.

También podemos querer obtener información sobre los permisos del directorio. Para ello, podemos utilizar la función `os.access()` que acepta una ruta y un modo de acceso como argumentos y nos devuelve `True` si la ruta tiene los permisos adecuados.

Existen otras funciones en la librería `os` que nos pueden proporcionar más información sobre un directorio, como por ejemplo `os.stat()` que nos devuelve información detallada, como el tamaño y la fecha de creación del directorio.

## Ver también

- Documentación de `os` en la librería estándar de Python: https://docs.python.org/es/3/library/os.html
- Tutorial de DigitalOcean sobre la comprobación de la existencia de directorios en Python: https://www.digitalocean.com/community/tutorials/how-to-check-if-a-directory-exists-in-python-3-es