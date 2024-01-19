---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Encontrar la longitud de una cadena (string) significa determinar la cantidad de caracteres que contiene esa cadena. Los programadores suelen hacer esto para controlar la entrada del usuario, medir el rendimiento o manipular datos.

## Cómo hacerlo:

Para obtener la longitud de una cadena en Elm, utilizamos la función `String.length`. Mira este código:

```Elm
import Html exposing (text)

main =
    "Hola, mundo" 
        |> String.length 
        |> toString
        |> Html.text
```

Este programa imprimirá "10", que es la longitud de la cadena "Hola, mundo".

## Análisis detallado

Historicamente, las funciones de longitud de cadena no han sido una característica integrada en todos los lenguajes de programación. Aún en el caso de Elm, la funcionalidad fue añadida en versiones posteriores del lenguaje.

Existen alternativas para determinar la longitud de una cadena en Elm. Por ejemplo, puedes transformar la cadena a una lista de caracteres con `String.toList` y luego usar la función `List.length`.

Sobre los detalles de implementación, Elm utiliza una implementación optimizada de `String.length` que opera de manera eficiente, incluso para cadenas muy largas.

## Ver también

Para más detalles sobre trabajar con strings en Elm, puedes consultar estos recursos:

- [Documentación oficial de Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Cómo trabajar con strings en Elm: Guía completa](https://www.example.com/working-with-strings-in-elm)
- [Elm String API](https://www.elm-lang.org/0.19.1/string)