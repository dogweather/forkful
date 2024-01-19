---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Convertir una cadena a minúsculas significa cambiar todos los caracteres en mayúsculas a minúsculas. Los programadores a menudo lo hacen para mantener la consistencia de los datos y facilitar las comparaciones sin distinción entre mayúsculas y minúsculas.

## ¿Cómo hacerlo? 
Aquí tienes un ejemplo directo. En Elm, puedes usar la función `String.toLower`:

```Elm
import Html exposing (text)
import String

main =
  text (String.toLower "HOLA MUNDO")
```
Esto devolverá `"hola mundo"`.

## Inmersión profunda
La función `String.toLower` ha existido desde los primeros días de Elm. Aunque es simple y eficaz, solo funciona con el alfabeto inglés. Para idiomas que contienen caracteres especiales es posible que necesites una solución personalizada.

Además, como alternativa, podrías utilizar la función `String.toLowercase` de JavaScript a través de ports si estás integrando Elm en un proyecto más grande de JavaScript.

## Ver también
Para más información acerca de `String.toLower` y otras funciones de string en Elm, puedes consultar la documentación oficial de Elm:

1. [Elm String Module](https://package.elm-lang.org/packages/elm/core/latest/String)
2. [Elm Guide](https://guide.elm-lang.org/)
3. En caso de necesitar más información sobre la integración de Elm y JavaScript: [Elm JavaScript Interop](https://guide.elm-lang.org/interop/)