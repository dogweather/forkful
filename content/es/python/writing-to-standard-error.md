---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Python: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Escribir a la salida de error estándar es una forma de mostrar mensajes de error en la consola mientras se ejecuta un programa. Los programadores lo hacen para depurar o encontrar problemas en su código de manera más eficiente.

## Cómo:
```
Python import sys
sys.stderr.write("¡Hola Mundo!")
```

Salida: ¡Hola Mundo!

## Profundizando:
Escribir a la salida de error estándar se remonta a los primeros días de la programación. Antes, los programadores solo tenían una forma de salida, la salida estándar. Esto dificultaba la depuración de errores. Con el tiempo, se agregó la capacidad de escribir a la salida de error estándar como una forma más específica de mostrar mensajes de error.

Una alternativa al uso de ```sys.stderr.write()``` es imprimir los mensajes de error usando la función ```print()``` y especificando el argumento ```file=sys.stderr```. Esta es una forma más conveniente y legible de escribir a la salida de error estándar.

La implementación de escribir a la salida de error estándar varía según el sistema operativo y el lenguaje de programación utilizado. En Python, se usa la función ```sys.stderr.write()``` y en sistemas operativos UNIX, se puede realizar utilizando la función ```fprintf()```.

## Ver también:
- Documentación oficial de Python: https://docs.python.org/es/3/library/sys.html#sys.stderr
- Tutorial de Python: https://docs.python.org/es/3/tutorial/inputoutput.html#error-handling
- Conceptos básicos de la salida estándar y de error: https://es.wikipedia.org/wiki/Salida_est.C3.A1ndar