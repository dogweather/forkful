---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:58:07.077284-07:00
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Verificar si un directorio existe en Python es básicamente preguntarle a tu sistema: "Oye, ¿este directorio que estoy nombrando está aquí?" Los programadores hacen esto para evitar errores al intentar acceder a directorios que no existen, para crearlos si es necesario, o para tomar decisiones basadas en su presencia o ausencia.

## Cómo se hace:

Usaremos el módulo `os` y `pathlib` para mostrar dos formas comunes de hacerlo.

```Python
import os

# Verificar la existencia del directorio con os.path.exists()
directorio = '/ruta/al/directorio'
if os.path.exists(directorio):
    print(f"¡Sí! El directorio {directorio} existe.")
else:
    print(f"No, el directorio {directorio} no existe.")
```

Usando `pathlib` que es más moderno y OOP:

```Python
from pathlib import Path

# Verificar la existencia con pathlib
directorio = Path('/ruta/al/directorio')
if directorio.exists():
    print(f"¡Genial! El directorio {directorio} está presente.")
else:
    print(f"Vaya, el directorio {directorio} no se encuentra.")
```

La salida será una simple confirmación de la existencia o no del directorio.

## Análisis Profundo:

Históricamente, `os.path.exists()` ha sido la opción tradicional para verificar la existencia de archivos y directorios. Es parte del módulo `os`, que es una especie de caja de herramientas para interactuar con el sistema operativo. Pero, con Python 3.4 y posteriores, `pathlib` hace su entrada, ofreciendo una alternativa con una API orientada a objetos, que muchos encuentran más intuitiva y Pythonic.

Alternativas menos comunes incluyen usar `os.listdir()` o `os.scandir()` para iterar sobre entradas en una ruta específica y verificar si el directorio deseado está entre ellas. Sin embargo, esta manera puede ser menos eficiente.

En cuanto a detalles de implementación, `os.path.exists()` puede retornar `True` para enlaces simbólicos rotos, ya que verifica la existencia de una entrada en el sistema de archivos con ese nombre. Mientras tanto, `Path.exists()` de `pathlib` comprueba si el archivo o directorio al que apunta la ruta realmente existe, lo que puede ser una diferencia sutil pero importante.

## Vea También:

- Documentación oficial de `os.path`: https://docs.python.org/3/library/os.path.html
- Documentación oficial de `pathlib`: https://docs.python.org/3/library/pathlib.html
- Tutorial de `pathlib`, si te inclinas por lo más moderno: https://realpython.com/python-pathlib/
- Python 3's pathlib Module: Taming the File System: https://www.toptal.com/python/python-3s-pathlib-module
