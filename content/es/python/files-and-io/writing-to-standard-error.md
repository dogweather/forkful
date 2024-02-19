---
aliases:
- /es/python/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:12.336735-07:00
description: "Escribir en el error est\xE1ndar en Python trata sobre dirigir los mensajes\
  \ de error o diagn\xF3sticos de tu programa al flujo de error (`stderr`), separado\
  \ de\u2026"
lastmod: 2024-02-18 23:09:09.574257
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar en Python trata sobre dirigir los mensajes\
  \ de error o diagn\xF3sticos de tu programa al flujo de error (`stderr`), separado\
  \ de\u2026"
title: "Escribiendo en el error est\xE1ndar"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en el error estándar en Python trata sobre dirigir los mensajes de error o diagnósticos de tu programa al flujo de error (`stderr`), separado de la salida estándar (`stdout`). Los programadores hacen esto para diferenciar las salidas normales del programa de los mensajes de error, facilitando la depuración y el análisis de registros.

## Cómo hacerlo:
### Usando `sys.stderr`
El módulo incorporado `sys` de Python permite escribir explícitamente en `stderr`. Este enfoque es directo para mensajes de error simples o diagnósticos.

```python
import sys

sys.stderr.write('Error: Algo salió mal.\n')
```
Salida de muestra (a stderr):
```
Error: Algo salió mal.
```

### Usando la función `print`
La función `print` de Python puede redirigir su salida a `stderr` especificando el parámetro `file`. Este método es útil para aprovechar la amigabilidad de `print` mientras se manejan mensajes de error.
```python
from sys import stderr

print('Error: Fallo en el módulo.', file=stderr)
```
Salida de muestra (a stderr):
```
Error: Fallo en el módulo.
```

### Usando el módulo `logging`
Para una solución más completa, el módulo `logging` de Python puede dirigir mensajes a `stderr` y mucho más, como escribir en un archivo o personalizar el formato del mensaje. Este método es mejor para aplicaciones que requieren diferentes niveles de registro, formateo de mensajes o destinos.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: La conexión a la base de datos falló.')
```
Salida de muestra (a stderr):
```
ERROR:__main__:Error: La conexión a la base de datos falló.
```

### Bibliotecas de terceros: `loguru`
`loguru` es una biblioteca de terceros popular que simplifica el registro en aplicaciones Python. Automáticamente dirige los errores a `stderr`, entre otras características.

Para usar `loguru`, primero instálalo a través de pip:
```shell
pip install loguru
```

Luego, incorpóralo en tu script de Python de la siguiente manera:
```python
from loguru import logger

logger.error('Error: No se pudo abrir el archivo.')
```
Salida de muestra (a stderr):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: No se pudo abrir el archivo.
```
