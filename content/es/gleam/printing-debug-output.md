---
title:                "Imprimiendo salida de depuración"
html_title:           "Gleam: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imprimir la salida de depuración es una técnica utilizada por los programadores para obtener información sobre el estado del programa en tiempo de ejecución. Esta información es útil para identificar errores y mejorar el rendimiento del código.

## ¡Cómo hacerlo!

Puedes imprimir la salida de depuración en Gleam utilizando la función `Debug.print`. Aquí hay un ejemplo:

```Gleam
let message = "¡Hola, mundo!"

let _ = Debug.print(message)
```

La salida resultante será:

```
"¡Hola, mundo!"
```

## Inmersión profunda

La impresión de salida de depuración ha sido una técnica ampliamente utilizada por los programadores desde los primeros días de la programación. Sin embargo, también existen otras formas de depurar un programa, como el uso de un depurador o la implementación de pruebas unitarias.

En Gleam, la función `Debug.print` utiliza macros en lugar de funciones regulares para mejorar el rendimiento y minimizar la sobrecarga en el código resultante.

## Lee también

- Documentación oficial de Gleam: https://gleam.run/documentation/
- Artículo de la revista Wired sobre la importancia de la impresión de salida de depuración: https://www.wired.com/story/importance-debugging-output/
- Tutorial de Codecademy sobre la depuración en Gleam: https://www.codecademy.com/learn/learn-gleam/modules/debugging-in-gleam