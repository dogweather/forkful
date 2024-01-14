---
title:    "Python: Comprobando si existe un directorio"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
Al programar en Python, puede ser útil verificar si un directorio existe antes de realizar operaciones en él. Esto puede ayudar a evitar errores y asegurar que su código se ejecute de manera eficiente.

## How To
```Python
import os

# Utilice el método "path.exists" del módulo "os" para comprobar si el directorio existe
if os.path.exists("/mi/directorio"):
    print("El directorio existe")
else:
    print("El directorio no existe")
```
```Python
# También puede usar el método "path.isdir" para verificar si el directorio es una carpeta válida
if os.path.isdir("/mi/directorio"):
    print("El directorio es una carpeta válida")
else:
    print("El directorio no es una carpeta válida")
```

Al ejecutar este código, si el directorio existe, se imprimirá "El directorio existe". Si el directorio no existe, se imprimirá "El directorio no existe". Si el directorio es una carpeta válida, se imprimirá "El directorio es una carpeta válida". Si el directorio no es una carpeta válida, se imprimirá "El directorio no es una carpeta válida".

## Deep Dive
Para verificar si un directorio existe, Python utiliza el método "path.exists" que devuelve un valor booleano (True/False) dependiendo de si el camino especificado existe o no. Esto significa que también puede usar este método para verificar si un archivo o enlace simbólico existe. Además, el método "path.isdir" es más específico y solo devuelve True si el camino especificado es una carpeta válida.

## Ver también
- Documentación oficial de Python: https://docs.python.org/es/3/library/os.path.html
- Tutorial de Linux: https://www.makeuseof.com/tag/how-to-check-if-a-directory-exists-in-python/
- Tutorial de Programiz: https://www.programiz.com/python-programming/directory