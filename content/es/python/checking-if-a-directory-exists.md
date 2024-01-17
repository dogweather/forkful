---
title:                "Comprobando si existe un directorio"
html_title:           "Python: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se hace?

Comprobar si un directorio existe es una tarea común en la programación en Python. Se utiliza para confirmar si una carpeta específica existe en el sistema antes de realizar alguna operación en ella. Esto puede evitar errores y mejorar la eficiencia en el código.

## ¿Cómo hacerlo?

Para verificar si un directorio existe en Python, podemos utilizar la función `os.path.exists()` que devuelve `True` si el directorio existe y `False` si no existe. A continuación se muestra un ejemplo de cómo comprobar si la carpeta "Documentos" existe en el directorio actual:

```Python
import os
if os.path.exists("Documentos"):
  print("¡El directorio existe!")
else:
  print("¡El directorio no existe!")
```

Si queremos confirmar que el directorio es realmente una carpeta y no un archivo, podemos utilizar `os.path.isdir()` que también devuelve `True` o `False`. Por ejemplo:

```Python
if os.path.isdir("Documentos"):
  print("¡Es una carpeta!")
else:
  print("¡No es una carpeta!")
```

## Profundizando

La función `os.path.exists()` se basa en el módulo `os.path` de Python, que ofrece una amplia gama de funciones para manipular rutas y nombres de archivo. Además, existen otras formas de comprobar si un directorio existe, como utilizar la función `os.listdir()` para obtener una lista de todos los elementos en un directorio y luego buscar el directorio en esa lista.

También hay módulos externos, como `pathlib`, que proporcionan una forma más moderna de interactuar con rutas y archivos en Python.

## Ver también

- Documentación oficial sobre el módulo `os.path` en Python: https://docs.python.org/3/library/os.path.html
- Documentación de `pathlib`: https://docs.python.org/3/library/pathlib.html