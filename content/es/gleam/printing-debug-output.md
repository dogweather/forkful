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

## Por qué

¿Alguna vez te has encontrado en la situación de tener que buscar y corregir errores en tu código? Si es así, entonces sabes lo valioso que es el proceso de depuración. Sin embargo, puede ser difícil encontrar el origen de un error, especialmente en programas más complejos. En este caso, imprimir mensajes de depuración en la consola puede ser una herramienta útil para ayudarte a entender lo que está sucediendo en tu código y dónde puede estar el error.

## Cómo Hacerlo

La versión actual de Gleam ha agregado una nueva función que facilita la impresión de mensajes de depuración en la consola. Aquí hay un ejemplo simple de cómo imprimir un mensaje en Gleam:

```
Gleam.debug("Este es un mensaje de depuración")
```

Este mensaje se mostrará en la consola cuando ejecutes tu código. También puedes imprimir valores de variables en tus mensajes de depuración para tener una mejor comprensión de lo que está sucediendo en tu código:

```
let nombre = "Juan"
Gleam.debug("Hola, mi nombre es #{@nombre}")
```

Esto imprimirá "Hola, mi nombre es Juan" en la consola. También puedes utilizar esta función en estructuras condicionales para imprimir mensajes de depuración solo en ciertas situaciones.

## Inmersión Profunda

La función de depuración en Gleam es útil para imprimir mensajes en la consola, pero también tiene algunas características únicas que pueden ser de ayuda. Por ejemplo, puedes utilizar variables de entorno para activar o desactivar la impresión de mensajes de depuración en diferentes entornos, como producción o desarrollo. También puedes utilizar la función `Gleam.log` para imprimir mensajes de depuración en un nivel de registro específico, lo que puede ser útil para filtrar y organizar tus mensajes en la consola.

## Ver También

- Documentación oficial de Gleam sobre la función de depuración: https://gleam.run/book/core-modules#debug
- Tutorial de Gleam sobre cómo utilizar la depuración en tu código: https://gleam.run/tour/debugging