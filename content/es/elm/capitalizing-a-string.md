---
title:                "Capitalizando una cadena de texto"
html_title:           "Elm: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena significa convertir la primera letra de la cadena a su forma mayúscula. Los programadores lo hacen para mejorar la claridad y legibilidad.

## Cómo hacerlo:

Para capitalizar una cadena en Elm, usaremos la función `toUpperCase` del módulo `String`, junto con `left` y `dropLeft` para manipular el primer carácter.

```Elm
capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            ""

        Just ( first, rest ) ->
            String.toUpper (String.fromChar first) ++ rest
```

En el código de arriba, `String.uncons` divide la cadena en su primer carácter y el resto. Si `str` está vacío (`Nothing`), devolvemos una cadena vacía. De lo contrario, convertimos el primer carácter a mayúsculas y lo concatenamos con el resto.

Ejemplo:

```Elm
capitalize "hola mundo"
```

Salida:

```Elm
"Hola mundo"
```

## Profundizando

La manipulación de cadenas es una tarea común en la programación, remontándose a los primeros días de la computación. Al manipular cadenas en Elm, es importante recordar que las cadenas son inmutables y que cada operación de manipulación produce una nueva cadena.

Si bien hemos utilizado el módulo `String` de Elm para esta tarea, existen otras funciones y módulos que también podrían usarse para resolverlo, como la librería `elm-string-extra`.

En términos de implementación, Elm instancia internamente un motor de JavaScript para realizar operaciones en las cadenas. Sin embargo, esto es transparente para los programadores de Elm, al igual que la mayoría de los detalles de implementación del lenguaje.

## Ver También

Para más información sobre las cadenas y sus usos en Elm, puede consultar los siguientes recursos:

- [Documentación oficial de Elm - String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm String Extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- [Stack Overflow - Elm string manipulation](https://stackoverflow.com/questions/tagged/elm+string)