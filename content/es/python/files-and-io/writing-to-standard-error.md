---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:12.336735-07:00
description: "C\xF3mo hacerlo: #."
lastmod: '2024-03-13T22:44:58.630318-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

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
