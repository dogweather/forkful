---
title:                "Elixir: Convirtiendo una cadena a minúsculas"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Aprender a convertir una cadena de texto a minúsculas es esencial para cualquier programador de Elixir. Esta habilidad te permitirá manipular cadenas de texto de una manera más eficiente y mejorar la calidad de tus aplicaciones.

## Cómo hacerlo

```elixir
# Creamos una cadena de texto
string = "Elixir es increíble"

# Utilizamos la función String.downcase() para convertir la cadena a minúsculas
String.downcase(string)

# Output: "elixir es increíble"
```

Puedes usar esta función para trabajar con cadenas de texto que contengan letras mayúsculas, minúsculas y caracteres especiales. Además, puedes aplicarla a concatenaciones de cadenas de texto o utilizarla en combinación con otras funciones para manipular de manera aún más eficiente tus cadenas de texto.

## Profundizando

La función `String.downcase()` es una forma sencilla de convertir una cadena de texto a minúsculas, pero también hay otras formas de lograr el mismo resultado. Por ejemplo, puedes utilizar la función `String.to_lower()` que, además de convertir a minúsculas, también se encarga de manejar los caracteres acentuados.

También puedes utilizar patrones e incluso el operador `|>` para encadenar varias funciones y convertir una cadena de texto a minúsculas de forma más compleja.

¡Explora diferentes opciones y encuentra la forma que mejor se adapte a tus necesidades!

## Ver también

- Documentación oficial de Elixir: https://elixir-lang.org/getting-started/string.html#downcasing-strings
- Ejemplos de conversión de strings en Elixir: https://hexdocs.pm/elixir/String.html#module-downcasing-and-upcasing

## Option 2:

## Por qué

Saber cómo convertir una cadena de texto a minúsculas es fundamental para cualquier persona interesada en aprender Elixir. Esta habilidad te permitirá manipular de forma eficiente cadenas de texto y mejorar la calidad de tus aplicaciones.

## Cómo hacerlo

```elixir
# Creamos una cadena de texto
texto = "Elixir es genial"

# Utilizamos la función String.downcase() para convertir la cadena a minúsculas
String.downcase(text)

# Output: "elixir es genial"
```

Esta función puede ser aplicada en cadenas de texto que contengan mayúsculas, minúsculas y caracteres especiales. Además, puedes utilizarla en concatenaciones de cadenas de texto o combinarla con otras funciones para manipular tus cadenas de texto de manera más eficiente.

## Profundizando

La función `String.downcase()` es una forma sencilla de convertir una cadena de texto a minúsculas, pero también existen otras opciones para lograr el mismo resultado. Por ejemplo, puedes utilizar `String.to_lower()` que, además de convertir a minúsculas, maneja los caracteres acentuados.

Además, puedes utilizar patrones e incluso el operador `|>` para encadenar varias funciones y realizar una conversión más compleja de una cadena de texto a minúsculas.

¡Explora diferentes opciones y encuentra la que mejor se adapte a tus necesidades!

## Ver también

- Documentación oficial de Elixir: https://elixir-lang.org/getting-started/string.html#downcasing-strings
- Ejemplos de conversión de strings en Elixir: https://hexdocs.pm/elixir/String.html#module-downcasing-and-upcasing