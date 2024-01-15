---
title:                "Capitalizando una cadena"
html_title:           "Gleam: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena?

Capitalizar una cadena es una operación común en la programación para asegurarse de que la primera letra de cada palabra esté en mayúsculas. Esto puede ser útil para mejorar la legibilidad de una cadena o para formatear correctamente los títulos en una aplicación.

## Cómo hacerlo

Para capitalizar una cadena en Gleam, se puede utilizar la función `String.capitalize` que viene incluida en su biblioteca estándar. A continuación se muestra un ejemplo de código y su resultado utilizando esta función:

```Gleam
let cadena = "hola mundo"
let cadena_capitalizada = String.capitalize(cadena)
```

El resultado será `"Hola mundo"`.

## Profundizando en la capitalización de cadenas

Además de la función `String.capitalize`, también existen otras formas de capitalizar cadenas en Gleam. Por ejemplo, se puede utilizar la función `String.titleize` para capitalizar todas las palabras de una cadena. Además, es posible utilizar la función `String.sentence_case` para capitalizar solo la primera letra de una cadena.

En Gleam también se pueden utilizar patrones de coincidencia de patrones en lugar de usar directamente funciones para realizar la capitalización. Esto permite una mayor flexibilidad en términos de qué partes de la cadena se capitalizan.

## Ver también

- Documentación oficial de la función `String.capitalize`: https://gleam.run/modules/gleam_std.String.html#capitalize
- Tutorial de Gleam en español: https://gleam.run/tour/es/ 
- Examples directory: https://github.com/gleam-lang/gleam/tree/master/examples