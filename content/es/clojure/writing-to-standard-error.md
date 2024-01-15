---
title:                "Escribiendo en error estándar"
html_title:           "Clojure: Escribiendo en error estándar"
simple_title:         "Escribiendo en error estándar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar es una práctica común en programación. Puede ser útil cuando se necesita registrar errores o advertencias en tiempo de ejecución, lo que facilita la depuración y el seguimiento de problemas en el código.

## Cómo hacerlo

Escribir a la salida de error estándar en Clojure es muy sencillo. Simplemente se utiliza la función "eprintln" para imprimir un mensaje en la consola de errores. Aquí hay un ejemplo de cómo imprimir un mensaje de error en Clojure:

```Clojure
(eprintln "¡Oops! Ha habido un error.")
```

Salida: ¡Oops! Ha habido un error.

También se pueden imprimir múltiples valores separados por comas:

```Clojure
(eprintln "El resultado es" 5 "+" 3 "=" 8)
```

Salida: El resultado es 5 + 3 = 8

## Profundizando

La función "eprintln" es un atajo para la función "print-to" que recibe como primer argumento la salida a la que se va a imprimir, en este caso la salida de error estándar.

Además de "eprintln", también existen otras funciones relacionadas con la escritura a la salida de error estándar en Clojure: "err", que permite imprimir un mensaje sin salto de línea al final, y "err-str", que devuelve el mensaje como una cadena en lugar de imprimirlo directamente.

También es importante mencionar que en Clojure, la salida de error estándar es compartida con la salida de salida estándar. Por lo tanto, se pueden usar las funciones "print" y "println" para imprimir a la salida de error estándar, aunque no es recomendable ya que puede confundir a los desarrolladores acostumbrados a usar "eprintln".

## Ver también

- [La función "eprintln" en la documentación de Clojure](https://clojuredocs.org/clojure.core/eprintln)
- [Cómo depurar programas en Clojure](https://www.freecodecamp.org/news/how-to-debug-programs-in-clojure/)