---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Elm: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir en la salida estándar de error puede ser una práctica útil para determinados casos en Elm. Por ejemplo, al depurar un código o buscar errores en una aplicación, mostrar mensajes de error en la salida estándar de error puede facilitar la identificación de problemas y su solución.

## Cómo hacerlo

Para imprimir en la salida estándar de error en Elm, se utiliza la función `Debug.crash` junto con el mensaje que se desea mostrar. Por ejemplo:

```elm
Debug.crash "¡Ocurrió un error!"
```

Esto imprimirá el mensaje "¡Ocurrió un error!" en la salida estándar de error.

## Profundizando

Además de la función `Debug.crash`, existen otras formas de escribir en la salida estándar de error en Elm. Por ejemplo, se puede utilizar la función `Debug.log` para mostrar mensajes de error junto con valores de variables y así tener una mejor comprensión de lo que está sucediendo en el código.

Por otro lado, también es posible personalizar los mensajes de error que se muestran en la salida estándar de error utilizando la librería `elm-beautiful-errors`. Esta librería permite incluir estilos y colores en los mensajes de error, lo que facilita aún más la identificación de problemas en el código.

## Ver también
- [Documentación de Elm sobre depuración](https://guide.elm-lang.org/debugging/)
- [Librería elm-beautiful-errors](https://package.elm-lang.org/packages/elm-community/elm-beautiful-errors/latest/)