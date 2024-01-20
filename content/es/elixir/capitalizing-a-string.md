---
title:                "Capitalizando una cadena de texto"
html_title:           "Elixir: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Elixir y El Arte de Capitalizar Cadenas de Texto

## ¿Qué & Por Qué?

Capitalizar una cadena de texto significa convertir su primera letra en mayúscula. Los programadores lo hacen para mejorar la legibilidad o para cumplir convenciones estilísticas.

## Cómo se hace:

Aquí te muestro un pedazo de código ilustrativo. Esto es cómo capitalizarías una cadena de texto en Elixir:

```Elixir
"elixir es asombroso"
|> String.split
|> Enum.map(&String.capitalize/1)
|> Enum.join(" ")
```

La salida sería:

```Elixir
"Elixir Es Asombroso"
```

## Un Buceo Más Profundo:

A lo largo de la historia, los lenguajes de programación han proporcionado diferentes formas de capitalizar una cadena de texto. En Elixir, puedes usar funciones integradas como `String.capitalize/1`. No obstante, si estás trabajando con cadenas más grandes o necesitas un mayor control, puedes utilizar la combinación de `String.split` y `Enum.map`.

Alternativamente, si tienes una cadena en español con caracteres no ASCII, podrías usar `:unicode.characters_to_nfc_binary/1` para obtener una capitalización adecuada. Sin embargo, esta función actualmente no soporta todas las características de capitalización del español.

La implementación de `String.capitalize/1` en Elixir utiliza la función `downcase` de la biblioteca estándar de Erlang para convertir toda la cadena a minúsculas antes de hacer la primera letra mayúscula.

## Ver También:

Para leer más sobre las funciones de cadena de Elixir, visita la [documentación oficial](https://hexdocs.pm/elixir/String.html). La [documentación de Erlang](https://erlang.org/doc/man/string.html) también puede resultar útil para aquellos interesados en entender las interacciones más bajas del lenguaje.