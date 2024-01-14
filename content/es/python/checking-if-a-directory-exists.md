---
title:                "Python: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

Comprobar si un directorio existe es una tarea esencial en la programación de Python, ya que permite verificar si un directorio o carpeta específica está presente en un sistema de archivos. Esto puede ser útil para asegurarse de que se esté accediendo al directorio correcto antes de realizar operaciones en él, como crear o borrar archivos.

## Cómo hacerlo

Comprobar si un directorio existe en Python es bastante sencillo gracias al módulo `os`. Primero, importamos el módulo en nuestro código:

```Python
import os
```

Luego, utilizamos la función `path.exists()` del módulo `os` y le pasamos como argumento la ruta del directorio que queremos comprobar. Si el directorio existe, la función devolverá `True` y si no existe, devolverá `False`. Veamos un ejemplo:

```Python
if os.path.exists("/ruta/al/directorio"):
    print("El directorio existe")
else:
    print("El directorio no existe")
```

En este ejemplo, estamos verificando si el directorio "/ruta/al/directorio" existe y mostrando un mensaje de acuerdo al resultado. Es importante tener en cuenta que esta función solo comprueba la existencia del directorio, no si es legible o si tenemos permisos para acceder a él.

## Inmersión profunda

Existen otras funciones disponibles en el módulo `os` que nos permiten obtener más información sobre un directorio en particular. Por ejemplo, la función `listdir()` nos devuelve una lista de los archivos y directorios dentro de un directorio específico. Además, podemos utilizar la función `isdir()` para comprobar si un elemento en la lista es un directorio o no.

También podemos utilizar la biblioteca `pathlib` para trabajar con rutas y directorios de una manera más intuitiva y sencilla. Esta biblioteca es compatible con versiones de Python posteriores a la 3.4 y nos permite comprobar la existencia de un directorio usando la sintaxis de objetos.

## Ver también

- Documentación oficial del módulo `os`: https://docs.python.org/es/3/library/os.html
- Documentación oficial de la biblioteca `pathlib`: https://docs.python.org/es/3/library/pathlib.html