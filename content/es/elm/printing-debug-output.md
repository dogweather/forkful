---
title:    "Elm: Imprimiendo salida de depuración"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Imprimir mensajes de depuración o "debug output" es una herramienta útil para el desarrollo de software. Nos permite ver los valores de las variables y el flujo del programa en tiempo real, lo que nos ayuda a identificar y solucionar errores en nuestro código.

## Cómo hacerlo

En Elm, podemos imprimir mensajes de depuración utilizando la función `Debug.log`. Por ejemplo, supongamos que queremos imprimir el resultado de una suma en nuestro programa:

```Elm
import Debug exposing (log)

sumar : Int -> Int -> Int
sumar x y =
    x + y

main =
    sumar 10 5
        |> Debug.log "Resultado de la suma"
```

Al ejecutar este código, veremos en la consola del navegador el siguiente mensaje:

`Resultado de la suma: 15`

Podemos utilizar esta técnica en cualquier lugar de nuestro código, incluso en funciones que toman múltiples argumentos o en llamadas a funciones anidadas.

## Profundizando

Además de imprimir valores, también podemos utilizar `Debug.log` para evaluar expresiones en nuestro código. Por ejemplo, si queremos saber el valor de una variable en un determinado punto de nuestro programa, podemos imprimirlo de la siguiente manera:

```Elm
nombre : String
nombre = "Juan"

apellido : String
apellido = nombre ++ " Pérez"

main =
    apellido
        |> Debug.log "Apellido"
```

Al ejecutar este código, el mensaje que veremos en la consola será:

`Apellido: "Juan Pérez"`

Esta técnica de imprimir mensajes de depuración es especialmente útil en Elm ya que, al ser un lenguaje funcional puro, no podemos utilizar `console.log` como en otros lenguajes.

## Ver también

- [Documentación de Elm sobre `Debug.log`](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
- [Artículo en español sobre técnicas de depuración en Elm](https://medium.com/@juanomaly/depurando-en-elm-analyzing-debugging-techniques-ff1b920196c2)
- [Guía de depuración en Elm en inglés](https://elmprogramming.com/debugging-elm-programs.html)