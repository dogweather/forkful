---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Extraer subcadenas es un procedimiento para obtener una cadena más corta de una cadena más larga. Los programadores lo hacen para manipular y analizar datos de manera eficaz.

## Cómo Hacerlo:

No existe una función incorporada en Elm para extraer subcadenas. Sin embargo, puedes combinar las funciones `drop` y `take` de la biblioteca `String` para lograrlo. Por ejemplo:

```Elm
import String exposing (drop, take)

extraerSubstring : String -> Int -> Int -> String
extraerSubstring cadena inicio longitud =
    cadena
    |> drop inicio
    |> take longitud
```

Uso:

```Elm
extraerSubstring "Programación" 0 5
-- "Progr"

extraerSubstring "Programación" 4 8
-- "amación"
```
## Análisis Profundo:

Extraer subcadenas es un concepto que ha existido desde los primeros días de la programación, y es una parte clave en manipular cadenas. En Elm, la carencia de una función directa para extraer subcadenas sugiere que se prefiere un enfoque más funcional, como combinar las funciones `take` y `drop`.

Existen alternativas para extraer subcadenas en Elm. Por ejemplo, se pueden utilizar funciones recursivas personalizadas o convertir una cadena en una lista de caracteres, operar en la lista y luego convertirla de nuevo en una cadena.

La implementación interna del `take` y `drop` en Elm es eficiente, ya que evita la creación de nuevas cadenas hasta que sea absolutamente necesario.

## Ver También:

- Documentación de String en Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- Biblioteca de funciones básicas de cadena: https://elmprogramming.com/basic-string-functions.html
- Transformación de listas y cadenas en Elm: https://elmprogramming.com/transforming-lists-and-strings.html