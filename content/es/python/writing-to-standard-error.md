---
title:                "Python: Escribiendo en el error estándar"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué: Escribir a la salida de error estandar

Escribir a la salida de error estándar en Python es una forma útil de depurar y diagnosticar errores en nuestro código. También puede ayudar a mejorar la legibilidad y organización del código al separar la salida de error de la salida estándar.

## Cómo hacerlo

Usar la función `print()` es una forma sencilla de imprimir a la salida estándar en Python. Para escribir a la salida de error estándar, simplemente usamos la función `print()` con el argumento `file=sys.stderr`.

Por ejemplo:

```Python
import sys
print("Este es un mensaje a la salida estándar")
print("Este es un mensaje a la salida de error estándar", file=sys.stderr)
```

La salida de este código sería:

```
Este es un mensaje a la salida estándar
Este es un mensaje a la salida de error estándar
```

## Profundizando

Es importante tener en cuenta que la salida de error estándar se utiliza para mensajes de error, por lo que se supone que estos mensajes serán leídos por desarrolladores o usuarios avanzados. Por esta razón, se recomienda usar una sintaxis clara y descriptiva al escribir a la salida de error.

Además, si se está utilizando un entorno de desarrollo integrado (IDE), puede haber opciones para ver y filtrar la salida de error estándar. Asegúrese de revisar la documentación de su IDE para conocer sus características y cómo utilizarlas para depurar su código.

## Ver también

- [Documentación de Python sobre la función print()](https://docs.python.org/es/3.9/library/functions.html#print)
- [Preguntas frecuentes sobre la salida de error estándar en Python](https://docs.python.org/es/3.9/faq/programming.html#how-do-i-write-to-standard-error)