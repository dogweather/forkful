---
title:                "Elm: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué usar expresiones regulares en Elm?

Si eres un desarrollador de Elm, probablemente estés familiarizado con la necesidad de manipular y validar cadenas de texto en tus aplicaciones. En lugar de escribir funciones complicadas para esto, puedes usar expresiones regulares para hacer el trabajo de manera más simple y eficiente. ¡Sigue leyendo para descubrir cómo hacerlo!

## Cómo usar expresiones regulares en Elm

Primero, debes importar el módulo `Regex` en tu archivo Elm:

```Elm
import Regex exposing (..)
```

Ahora, puedes usar la función `Regex.contains` para verificar si una cadena de texto contiene un patrón dado:

```Elm
Regex.contains (Regex.regex "hola") "¡Hola, mundo!" -- Devuelve True
```

También puedes usar la función `Regex.replace` para reemplazar un patrón en una cadena de texto con otra cadena:

```Elm
Regex.replace (Regex.regex "\\d+") "El número es 123" "XX" -- Devuelve "El número es XX"
```

## Detalles sobre el uso de expresiones regulares en Elm

Las expresiones regulares en Elm siguen la sintaxis de Perl. Puedes encontrar más información sobre los diferentes patrones y símbolos que puedes usar en la [documentación oficial de Elm Regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex). También puedes utilizar herramientas en línea como [RegExr](https://regexr.com/) o [Regex101](https://regex101.com/) para probar tus expresiones regulares antes de implementarlas en tu código Elm.

## Ver también

- [Documentación oficial de Elm Regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Tutorial de expresiones regulares de RegExr](https://blog.usejournal.com/learn-regular-expressions-by-building-a-tool-to-parse-lexical-annotations-93b5f31d4ab0)
- [Guía paso a paso de Regex101](https://www.youtube.com/watch?v=DRR9fOXkfRE)