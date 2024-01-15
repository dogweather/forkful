---
title:                "Escribiendo a error estándar"
html_title:           "Python: Escribiendo a error estándar"
simple_title:         "Escribiendo a error estándar"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en el error estándar?

Escribir en el error estándar es una práctica común en la programación de Python. Permite a los desarrolladores imprimir información sobre errores o excepciones que puedan ocurrir durante la ejecución de un programa, lo que facilita la identificación y solución de problemas.

## Cómo hacerlo

Para escribir en el error estándar en Python, puedes utilizar la función `print()` junto con la palabra clave `file=sys.stderr`. Esto especifica que la salida se redirigirá al error estándar en lugar de imprimirse en la consola.

```
import sys

print("Este mensaje se imprimirá en el error estándar", file=sys.stderr)
```

La salida del código anterior se verá así en la consola:

```
Este mensaje se imprimirá en el error estándar
```

## Profundizando

La principal diferencia entre escribir en la consola y escribir en el error estándar es que la consola estándar es utilizada para la salida del programa, mientras que el error estándar es utilizado para la salida de errores y excepciones. Esto significa que los mensajes de error no se mezclarán con la salida normal del programa.

Otra ventaja de escribir en el error estándar es que se puede redirigir a un archivo o registro para facilitar la solución de problemas y el seguimiento de errores.

## Ver también

Para más información sobre la escritura en el error estándar en Python, puedes revisar la documentación oficial:

- Documentación de Python en la función `print()`: https://docs.python.org/es/3/library/functions.html#print
- Guía de The Real Python sobre cómo escribir en el error estándar: https://realpython.com/python-logging/#the-logging-module
- Artículo de Real Python sobre cómo depurar errores en Python: https://realpython.com/python-debugging-pdb/