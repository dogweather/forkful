---
title:                "Buscando y reemplazando texto"
html_title:           "Elm: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Reemplazar texto es una tarea común en programación que consiste en buscar un determinado fragmento de texto y reemplazarlo por otro. Los programadores lo hacen para modificar cadenas de texto de manera más eficiente y automatizada.

## Cómo:
```
Elm.replace "hola" "adiós" "hola, mundo" --> "adiós, mundo"
```

En este ejemplo, utilizamos la función `replace` de Elm para buscar y reemplazar la palabra "hola" por "adiós" en la cadena "hola, mundo". El resultado es la cadena modificada "adiós, mundo".

## Profundizando
Reemplazar texto no es una técnica nueva en programación. De hecho, ha sido una herramienta esencial desde los primeros lenguajes de programación. Alternativas como expresiones regulares también son utilizadas para este propósito. En Elm, la función `replace` utiliza patrones de comprensión para realizar la búsqueda y reemplazo de manera más eficiente.

## Ver también
- Documentación de Elm sobre la función `replace`: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Información sobre expresiones regulares en Elm: https://guide.elm-lang.org/interop/regex.html