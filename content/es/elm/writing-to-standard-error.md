---
title:                "Elm: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar es una técnica común en la programación para mostrar información de errores y excepciones. Es particularmente útil para depurar y solucionar problemas en nuestro código. En este artículo, veremos cómo podemos hacer uso de la funcionalidad de escritura a la salida de error estándar en Elm.

## Cómo hacerlo

Para escribir a la salida de error estándar en Elm, utilizamos la función `Debug.log` seguida de dos argumentos separados por comas. El primer argumento es una etiqueta para identificar el mensaje y el segundo argumento es el mensaje que queremos imprimir.

```elm
import Debug

Debug.log "Error" "Este es un mensaje de error"
-- Error: Este es un mensaje de error
```

Podemos usar cualquier tipo de datos para el mensaje, incluyendo cadenas de texto, números o listas.

```elm
import Debug

Debug.log "Mensaje" 42
-- Mensaje: 42
```

También podemos incluir variables en nuestro mensaje usando la función `String.fromInt` para convertir números a cadenas de texto.

```elm
import Debug

let mensaje = "Este es un mensaje con una variable: " ++ (String.fromInt 42)

Debug.log "Mensaje" mensaje
-- Mensaje: Este es un mensaje con una variable: 42
```

Estos mensajes se imprimirán en la consola del navegador, lo que nos permite depurar y entender mejor nuestro código.

## Profundizando

Hay algunas cosas importantes que debemos tener en cuenta al escribir a la salida de error estándar en Elm. Primero, es importante recordar eliminar todas las llamadas a `Debug.log` antes de lanzar nuestra aplicación a producción. Estas llamadas pueden ralentizar el rendimiento de nuestra aplicación y también pueden revelar información sensible al usuario.

Además, como Elm es un lenguaje funcional puro, solo podemos utilizar esta técnica en nuestro código de depuración. No podemos usarla en nuestra lógica de producción ya que esto violaría la inmutabilidad y pureza de Elm.

## Ver también

- [Documentación oficial de Elm sobre Debugging](https://guide.elm-lang.org/debugging/)
- [Tutorial de Elm para principiantes](https://www.elm-tutorial.org/es/)
- [Canales oficiales de Elm en español en Telegram](https://t.me/ElmES)