---
title:    "Elm: Convertir una cadena a minúsculas."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser una tarea común en programación, especialmente cuando se trabaja con datos sensibles a mayúsculas y minúsculas. Aprender cómo hacerlo en Elm puede ser útil en diversas situaciones.

## Cómo hacerlo

En Elm, podemos convertir una cadena de texto a minúsculas utilizando la función `String.toLower`.

```
Elm import String


let cadena = "ELM ES FÁCIL DE APRENDER"

String.toLower cadena

// Output: "elm es fácil de aprender"
```

También podemos utilizar esta función en una cadena de texto introducida por el usuario, utilizando el paquete `elm/html` y el evento `onInput`.

```
Elm import Html exposing (..)
import Html.Events exposing (..)
import String

main =
  Html.div []
    [
      Html.input [ onInput Convertir ] [],
      Html.div [][TextoConvertido]
    ]

Convertir input =
  Modelo (String.toLower input)

TextoConvertido =
  Html.text modelo.textoConvertido
```

Al escribir en el campo de entrada, `onInput` activará la función `Convertir` que, al utilizar `String.toLower`, convertirá lo que se escriba en minúsculas. Luego, el texto convertido se mostrará en el `div` correspondiente con el atributo `Modelo`.

## Profundizando

Al utilizar `String.toLower`, es importante tener en cuenta que la función no solo convierte letras a minúsculas, sino que también puede cambiar la representación de caracteres Unicode y ASCII. Esto se debe a que Elm está basado en Unicode y utiliza la tabla de caracteres Unicode para convertir una cadena de texto a minúsculas.

Además, hay casos en los que `String.toLower` puede no funcionar como se espera, como en el caso de letras acentuadas o caracteres especiales. Para evitar esto, se pueden utilizar diferentes paquetes como `elm-community/string-extra` o `charlitos64/elm-i18n`, que proporcionan funciones específicas para manejar estos casos.

## Ver también

- [Referencia de la función `String.toLower` en la documentación oficial de Elm](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Paquete `elm-community/string-extra` en la biblioteca de paquetes de Elm](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- [Paquete `charlitos64/elm-i18n` en la biblioteca de paquetes de Elm](https://package.elm-lang.org/packages/charlitos64/elm-i18n/latest/)