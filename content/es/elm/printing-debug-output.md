---
title:    "Elm: Imprimiendo salida de depuración"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir salidas de depuración puede ser una herramienta útil para entender el comportamiento de nuestras aplicaciones Elm. Nos permite ver qué está sucediendo en el código en diferentes puntos y puede ayudarnos a identificar y solucionar problemas.

## Cómo hacerlo

Para imprimir salidas de depuración en Elm, podemos utilizar la función `Debug.log`. Esta función toma dos argumentos: una cadena de texto que será el mensaje de depuración y un valor que queremos imprimir. Por ejemplo:

```
Elm
    import Debug

    userName : String
    userName = "John"

    userAge : Int
    userAge = 25

    main =
        -- imprimir salidas de depuración de la variable `userName` y la variable `userAge`
        Debug.log "Nombre de usuario: " userName
        Debug.log "Edad del usuario: " userAge

```
La salida de la consola para este código sería:

```
Nombre de usuario: John
Edad del usuario: 25
```

Podemos utilizar `Debug.log` en cualquier lugar de nuestro código para imprimir salidas de depuración y entender mejor lo que está sucediendo en cada paso.

## Profundizando

Además de `Debug.log`, también podemos utilizar la función `Debug.toString` para imprimir valores en formato de cadena de texto. Esto nos permite imprimir valores más complejos, como listas y registros. Por ejemplo:

```
Elm
    import Debug

    capicua : List Int
    capicua = [1, 2, 3, 2, 1]

    main =
        -- imprimir salida de depuración de la lista `capicua`
        Debug.toString capicua

```

La salida de la consola para este código sería:

```
"[1,2,3,2,1]"
```

Otra herramienta útil para imprimir salidas de depuración en Elm es `Debug.watch`. Esta función nos permite imprimir salidas de depuración de forma interactiva y en tiempo real mientras nuestra aplicación se está ejecutando. Podemos utilizarla para ver los cambios en los valores de las variables a medida que interactuamos con nuestra aplicación.

## Ver también

- [Documentación de Elm Debug Module](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)
- [Video tutorial sobre depuración en Elm](https://www.elm-tutorial.org/en/06-debugging/01-debug-devtools.html)
- [Ejemplos de código de depuración en Elm](https://gist.github.com/nosajimiki/67557a2795ea1accdb039edca931f839)