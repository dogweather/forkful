---
title:                "Elixir: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La capitalización de una cadena es una tarea común en la programación, especialmente cuando se trabaja con cadenas de texto. Puede ser útil en situaciones como formatear nombres para que comiencen con mayúscula o para mostrar títulos correctamente.

## Cómo hacerlo

Para capitalizar una cadena en Elixir, podemos utilizar la función `String.capitalize/1` que se encuentra en el módulo `String`. Esta función toma una cadena como argumento y devuelve una nueva cadena con el primer carácter en mayúscula.

```elixir
String.capitalize("hola, mundo")
# Devuelve "Hola, mundo"
```

Si queremos capitalizar todas las palabras en una cadena, podemos utilizar la función `String.capitalize_words/1`, que también se encuentra en el módulo `String`.

```elixir
String.capitalize_words("bienvenido a elixir")
# Devuelve "Bienvenido A Elixir"
```

## Profundizando

Internamente, estas funciones utilizan el módulo `String.Case` para realizar la capitalización. Este módulo contiene funciones para manejar diferentes casos de formato, como mayúsculas, minúsculas y formato de texto.

Por ejemplo, para capitalizar solo la primera palabra en una cadena, podemos utilizar la función `String.Case.upcase/1`. Esta función devuelve una cadena con la primera letra en mayúscula, pero deja el resto de la cadena como estaba.

```elixir
String.Case.upcase("bienvenido")
# Devuelve "Bienvenido"
```

Otra forma de capitalizar una cadena es utilizando la función `String.replace/3` junto con expresiones regulares para seleccionar la primera letra de cada palabra y convertirla en mayúscula.

```elixir
String.replace("hola, mundo", ~r/(?<=\A|\s)(\w)/, fn letter -> String.upcase(letter) end)
# Devuelve "Hola, Mundo"
```

## Ver también

- Documentación oficial de Elixir sobre `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/1
- Tutoriales sobre expresiones regulares en Elixir: https://elixirschool.com/es/lessons/basics/pattern-matching/#regex-regular-expressions