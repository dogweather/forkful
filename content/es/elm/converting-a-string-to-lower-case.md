---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Elm: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué
La conversión de cadenas de texto a minúsculas es una tarea común en la programación que puede ser útil para estandarizar datos o para comparar cadenas de forma insensible a mayúsculas y minúsculas.

## Cómo hacerlo
Para realizar esta tarea en Elm, podemos utilizar la función `String.toLower` que toma una cadena de texto como argumento y devuelve una nueva cadena en minúsculas. A continuación, se muestra un ejemplo de código y su salida:

```Elm
cadena = "HOLA MUNDO"
String.toLower cadena
-- "hola mundo"
```

## Profundizando
La función `String.toLower` utiliza la tabla Unicode de conversión a minúsculas para realizar la transformación. Esto asegura que incluso los caracteres en idiomas diferentes al inglés sean convertidos correctamente. Además, también está disponible la función `String.toLowerWithLocale` que usa la configuración regional del sistema operativo para la conversión.

## Véase también
- Documentación oficial de `String.toLower` en la Guía del Programador de Elm: https://guide.elm-lang.org/appendix/unicode.html#to-lower
- Documentación oficial de `String.toLowerWithLocale` en la Guía del Programador de Elm: https://package.elm-lang.org/packages/elm/core/latest/String#toLowerWithLocale