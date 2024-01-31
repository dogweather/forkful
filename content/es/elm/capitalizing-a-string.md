---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
simple_title:         "Capitalizando una cadena de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programar lo hacen para normalizar textos, como nombres propios o títulos, asegurando consistencia y legibilidad.

## Cómo Hacerlo:

Elm no tiene una función incorporada para capitalizar automáticamente las strings, así que vamos a crear una nosotros mismos. El truco está en convertir la primera letra a mayúscula y luego concatenarla con el resto de la cadena.

```Elm
import String exposing (toUpper, toLower, right, left)

capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            ""

        Just ( first, rest ) ->
            toUpper (String.fromChar first) ++ toLower rest

-- Uso
main =
    String.split " " "hola mundo" -- separamos la cadena por espacios
    |> List.map capitalize -- capitalizamos cada palabra
    |> String.join " " -- reconcatenamos la cadena

-- Salida: "Hola Mundo"
```

## Análisis Profundo

Históricamente, la capitalización de cadenas de texto no es algo nuevo y ha sido aplicada en múltiples lenguajes de programación. En Elm, como es un lenguaje funcional, se prefiere manipular las cadenas de manera explícita mediante funciones. 

Además de la función `capitalize` que creamos, podemos tener alternativas que, por ejemplo, solo capitalicen la primera letra de toda la cadena, no cada palabra. Esto resulta útil para ciertos casos específicos donde no se requiere capitalizar todo.

Detalles de implementación: La clave aquí es la función `String.uncons`, que divide la cadena en su primer carácter y el resto. Luego usamos `toUpper` y `toLower` del módulo `String` para cambiar el caso de los caracteres. Es una solución eficiente y elegante que se adapta bien al estilo funcional de Elm.

## Ver También

Para más información sobre las funciones que utilizamos, puedes visitar la documentación oficial de Elm:

- String module: https://package.elm-lang.org/packages/elm/core/latest/String
- Función uncons: https://package.elm-lang.org/packages/elm/core/latest/String#uncons
- List.map: https://package.elm-lang.org/packages/elm/core/latest/List#map
