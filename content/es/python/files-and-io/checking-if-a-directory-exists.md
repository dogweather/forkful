---
title:                "Comprobando si un directorio existe"
date:                  2024-02-03T19:08:09.195586-07:00
model:                 gpt-4-0125-preview
simple_title:         "Comprobando si un directorio existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Verificar si existe un directorio en Python se trata de comprobar la presencia de una carpeta en el sistema de archivos antes de realizar operaciones como leer o escribir archivos. Los programadores hacen esto para evitar errores como `FileNotFoundError`, asegurando que la aplicación se comporte de manera confiable y no se cuelgue al intentar interactuar con directorios.

## Cómo hacerlo:
Python ofrece formas nativas de comprobar si existe un directorio utilizando los módulos `os` y `pathlib`. Aquí hay ejemplos para ambos:

### Usando el módulo `os`
```python
import os

# Especifica la ruta del directorio
dir_path = "/ruta/al/directorio"

# Verifica si el directorio existe
if os.path.isdir(dir_path):
    print(f"El directorio {dir_path} existe.")
else:
    print(f"El directorio {dir_path} no existe.")
```

### Usando el módulo `pathlib`
```python
from pathlib import Path

# Especifica la ruta del directorio
dir_path = Path("/ruta/al/directorio")

# Verifica si el directorio existe
if dir_path.is_dir():
    print(f"El directorio {dir_path} existe.")
else:
    print(f"El directorio {dir_path} no existe.")
```

### Bibliotecas de terceros
Aunque la biblioteca estándar de Python es suficiente para comprobar si existe un directorio, bibliotecas como `pathlib2` pueden ser alternativas para la consistencia a través de las versiones de Python o funcionalidad adicional.

***Nota:*** A partir de las últimas versiones de Python, `pathlib` es lo suficientemente robusto para la mayoría de los casos de uso, haciendo que las bibliotecas de terceros sean menos necesarias para esta tarea específica.