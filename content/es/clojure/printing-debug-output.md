---
title:                "Imprimiendo la salida de depuración"
html_title:           "Clojure: Imprimiendo la salida de depuración"
simple_title:         "Imprimiendo la salida de depuración"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La impresión de mensajes de depuración es un proceso en el que los programadores incluyen mensajes en su código para ayudar a identificar errores o problemas durante la ejecución. Esto les permite seguir el flujo de su programa y detectar posibles problemas antes de que se conviertan en errores críticos.

## Cómo hacerlo:

Para imprimir mensajes de depuración en Clojure, simplemente utilizamos la función *prn* seguida de los valores que deseamos imprimir. Por ejemplo:

```Clojure
(prn "Hola Mundo!")
(prn (+ 2 3))
```

Esto producirá la siguiente salida en la consola:

```Clojure
"Hola Mundo!"
5
```

## Profundizando:

La impresión de mensajes de depuración ha sido una práctica común en la programación desde los primeros días de la informática. En lugar de interrumpir la ejecución con un punto de interrupción, los programadores pueden utilizar la impresión de mensajes de depuración para seguir el flujo del programa y ver los valores de las variables en diferentes puntos del código.

Existen varias alternativas a la impresión de mensajes de depuración, como el uso de un depurador interactivo o una herramienta de visualización de datos. Sin embargo, la impresión de mensajes sigue siendo una técnica valiosa y sencilla que puede ser utilizada en cualquier momento.

En términos de implementación, la función *prn* es en realidad una forma abreviada de la función *println*, que es una función interna que imprime su argumento en la salida estándar. Además, la función *prn* también imprime una nueva línea después de cada argumento.

## Ver también:

- [Página de documentación de Clojure](https://clojure.org/)
- [Otra forma de imprimir mensajes en Clojure](https://clojuredocs.org/clojure.core/pr)
- [Uso de depuradores interactivos en Clojure](https://clojuredocs.org/stateful.html#important-note-on-the-use-of-a-debugger)