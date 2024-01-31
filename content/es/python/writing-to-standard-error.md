---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en el error estándar (stderr) permite mostrar mensajes de error y diagnóstico mientras el flujo normal (stdout) sigue libre para la salida de datos. Los programadores usan stderr para separar la lógica de errores de la salida principal y facilitar la depuración.

## Cómo hacerlo:
Python simplifica la escritura en stderr a través del módulo `sys`. A continuación, unos ejemplos:

```Python
import sys

print("Esto va a stdout")
sys.stderr.write("Esto va a stderr\n")

# Usando la función print
print("Otro mensaje de error", file=sys.stderr)
```

Salida esperada:
```
Esto va a stdout
Esto va a stderr
Otro mensaje de error
```

Notarás que "Esto va a stdout" aparecerá en la salida estándar, mientras que los mensajes de error lo harán en el error estándar.

## Profundizando:
Historia: Originalmente en los sistemas Unix, stderr fue diseñado para que los mensajes de error no se mezclaran con la salida de datos regular.

Alternativas: En vez de usar `sys.stderr`, se puede usar el módulo `logging` para una gestión más avanzada de mensajes de error y diagnóstico.

Detalles de implementación: Cuando escribes a stderr usando `sys.stderr.write()`, debes asegurarte de agregar tú mismo el carácter de nueva línea `\n`, mientras que `print()` lo añade automáticamente.

## Ver También:
- La documentación oficial de Python sobre E/S: https://docs.python.org/3/tutorial/inputoutput.html
- Tutorial sobre el módulo `logging`: https://docs.python.org/3/howto/logging.html
- Explicación de stdout y stderr en sistemas Unix: https://en.wikipedia.org/wiki/Standard_streams
