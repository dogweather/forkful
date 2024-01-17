---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Elm: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Eliminar caracteres que coinciden con un patrón es una técnica común en la programación que consiste en buscar y eliminar ciertos caracteres dentro de una cadena de texto. Los programadores utilizan esta técnica para limpiar o manipular datos de manera eficiente, ahorrando tiempo y esfuerzo en el proceso.

## Cómo:

```Elm
-- Definimos una función que reciba una cadena de texto y un patrón a eliminar
deletePattern : String -> String -> String
deletePattern text pattern =
    String.filter pattern (\c -> c /= pattern) text

-- Ejemplo de uso
deletePattern "Elm es genial!" "e" -- devolverá "Elm s gnil!"
```

## Inmersión profunda:

Esta técnica ha existido desde los primeros lenguajes de programación, como FORTRAN y COBOL. En otros lenguajes modernos, como JavaScript, se puede implementar utilizando expresiones regulares. En Elm, se utiliza la función `filter` del módulo `String` para buscar y eliminar los caracteres que coinciden con el patrón especificado.

Algunas alternativas a esta técnica pueden ser el uso de ciclos y condicionales para recorrer la cadena de texto y eliminar manualmente los caracteres, o el uso de bibliotecas externas que proporcionan funciones más avanzadas para manipular cadenas de texto.

## Ver también:

Puedes aprender más sobre la función `filter` y otras funciones relacionadas en la documentación oficial de Elm: [https://package.elm-lang.org/packages/elm-lang/core/latest/String](https://package.elm-lang.org/packages/elm-lang/core/latest/String)

También puedes explorar más sobre el manejo de cadenas de texto en Elm en el libro "Guía de programación de Elm": [https://guide.elm-lang.org/strings/](https://guide.elm-lang.org/strings/)