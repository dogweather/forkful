---
title:                "Elm: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir salidas de depuración en Elm?

Si estás empezando a aprender Elm, puede ser útil imprimir salidas de depuración para entender mejor cómo funciona tu código. También puede ser útil para identificar y solucionar errores en tu programa.

## Cómo imprimir salidas de depuración en Elm

Para imprimir una salida de depuración en Elm, puedes utilizar la función ```Debug.log``` y pasarle una etiqueta y un valor para imprimir. Aquí hay un ejemplo:

```Elm
myInt = 5
Debug.log "Valor de myInt:" myInt
```

Al ejecutar este código, verás en la consola la etiqueta y el valor que has impreso: "Valor de myInt: 5".

También puedes imprimir valores de variables en una lista o en un registro. Por ejemplo:

```Elm
myList = [1, 2, 3]
Debug.log "Lista:" myList

myRecord = { x = 1, y = 2 }
Debug.log "Registro:" myRecord
```
En la consola se imprimirán los valores de la lista y del registro:

"Lista: [1, 2, 3]"

"Registro: { x = 1, y = 2 }"

## Profundizando en la impresión de salidas de depuración

Una de las ventajas de utilizar la función ```Debug.log``` es que solo se imprimirá la salida de depuración en modo de desarrollo, no en producción. Esto significa que en tu aplicación en vivo, las salidas de depuración no serán visibles para los usuarios. Esto es útil para no sobrecargar tu aplicación con salidas innecesarias en producción.

Además, puedes utilizar esta función no solo para imprimir valores, sino también para comprobar si ciertos bloques de código están siendo ejecutados. Al pasar una cadena vacía como etiqueta, la salida de depuración se imprimirá sin la etiqueta, lo que puede ser útil para comprobar si un bloque de código en particular se está ejecutando.

## Ver también

- [Documentación oficial de Debug en Elm](https://guide.elm-lang.org/interop/debug.html)
- [Tutorial de Elm en español](https://www.youtube.com/watch?v=N2NWC6USYp8)
- [Ejemplos de código en Elm](https://github.com/elm-examples)

Gracias por leer este post sobre cómo imprimir salidas de depuración en Elm. Esperamos que te haya sido útil para comprender mejor esta herramienta y puedas utilizarla en tus proyectos de manera efectiva. ¡Hasta la próxima!