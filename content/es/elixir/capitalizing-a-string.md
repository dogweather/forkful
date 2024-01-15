---
title:                "Convirtiendo una cadena en mayúsculas"
html_title:           "Elixir: Convirtiendo una cadena en mayúsculas"
simple_title:         "Convirtiendo una cadena en mayúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Elixir?

Capitalizar una cadena de texto es una tarea común en la programación. Al capitalizar una cadena, se cambia la primera letra de cada palabra a mayúscula. Esto puede ser útil en situaciones como la conversión de un título en formato de título capitalizado. En Elixir, existe una función incorporada para capitalizar fácilmente una cadena, lo que ahorra tiempo y esfuerzo en la escritura de código personalizado.

## Cómo capitalizar una cadena en Elixir

El método más sencillo para capitalizar una cadena en Elixir es utilizando la función `String.capitalize/1`. Veamos un ejemplo de cómo usar esta función:

```elixir
iex> String.capitalize("hola mundo")
"Hola mundo"
```

En este ejemplo, introducimos una cadena de texto en minúsculas en la función `String.capitalize/1` y obtenemos una nueva cadena con la primera letra de cada palabra en mayúscula.

Si deseamos capitalizar solo la primera letra de una cadena, podemos utilizar la función `String.capitalize_first/1` de la siguiente manera:

```elixir
iex> String.capitalize_first("hola mundo")
"Hola mundo"
```

Otra forma de capitalizar una cadena es utilizando la función `IO.puts/1` para imprimir la cadena en la consola con su primera letra en mayúscula:

```elixir
iex> IO.puts("hola mundo")
"Hola mundo"
```

## Profundizando en la capitalización de cadenas en Elixir

La función `String.capitalize/1` utiliza la convención del sistema operativo para determinar qué caracteres se deben capitalizar. Por ejemplo, en inglés, la primera letra de cada palabra se capitalizará, pero en idiomas como español o francés, solo la primera letra de la primera palabra se capitalizará. Si deseamos especificar un idioma diferente al del sistema operativo, podemos utilizar la función `String.capitalize/2` y pasarle un mapa como segundo argumento, especificando las reglas de capitalización para el idioma deseado.

Otra función útil en Elixir para capitalizar cadenas es `String.capitalize_words/1`, que capitaliza la primera letra de cada palabra en una cadena dada. Esta función también acepta un argumento opcional `exclude`, que especifica qué palabras deben ser excluidas de la capitalización.

## Ver también

- [Documentación oficial de la función `String.capitalize/1` en Elixir](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Otra funcionalidad útil para trabajar con cadenas en Elixir: `String.split/2`](https://www.twilio.com/blog/2016/09/getting-started-with-elixir.html)
- [Cómo trabajar con cadenas en Elixir usando el módulo `String`](https://www.learningelixir.com/2018/10/17/working-on-strings/)