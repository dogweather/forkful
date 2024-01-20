---
title:                "Verificando si un directorio existe"
html_title:           "Gleam: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Verificar si un directorio existe en Python es simplemente comprobar si un determinado camino contiene un directorio. Los programadores lo hacen para evitar errores al intentar acceder a un directorio inexistente.

## ¿Cómo se hace?

Para verificar si un directorio existe en Python, usamos el módulo `os` y la función `os.path.exists`.

```Python
import os

# Directorio a verificar
dir_path = "/home/user/directory"

# Verificar si el directorio existe
exists = os.path.exists(dir_path)

print("¿El directorio existe?: ", exists)
```

Resultado:

```Python
¿El directorio existe?: True
```

Si el directorio existe, la función `os.path.exists` devolverá `True`. Si no existe, devolverá `False`.

## Buceo Profundo

La función `os.path.exists()` es parte del módulo `os` que ha estado en Python desde sus inicios. Antes, la gente tenía que usar llamadas del sistema para comprobar la existencia de un directorio, lo cual era más complicado y propenso a errores.

Alternativamente, Python también proporciona `os.path.isdir()` que no sólo verifica la existencia de un camino, sino que también comprueba si ese camino es un directorio.

```Python
import os

dir_path = "/home/user/directory"

# Verificar si es un directorio
is_directory = os.path.isdir(dir_path)

print("¿Es un directorio?: ", is_directory)
```

En lo que respecta a la implementación, `os.path.exists` usa la función `stat()` del sistema operativo subyacente, que obtiene los metadatos del fichero o directorio. Si `stat()` informa de un error, `os.path.exists` asume que el camino no existe.

## Ver También

Podrás encontrar más información en la [documentación oficial de Python](https://docs.python.org/3/library/os.path.html) y en esta [guía de manipulación de archivos y directorios en Python](https://realpython.com/working-with-files-in-python/) de Real Python.