---
title:                "Elixir: Capitalizando una cadena"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Uno de los aspectos más básicos en cualquier lenguaje de programación es la capacidad de manipular cadenas de texto. Una de estas tareas comunes es la capitalización de una cadena. En este artículo, exploraremos cómo podemos lograrlo utilizando Elixir.

## Cómo Hacerlo

Para capitalizar una cadena en Elixir, podemos utilizar la función `String.capitalize/1`, que acepta una cadena como argumento y devuelve una versión capitalizada de la misma. Veamos un ejemplo de código:

```elixir
String.capitalize("hola mundo")
```

Este código producirá la salida `"Hola mundo"`, con la primera letra de cada palabra en mayúscula. Podemos incluso capitalizar solo la primera letra de la cadena:

```elixir
String.capitalize("hola mundo", :first)
```

Esta vez, la salida será `"Hola mundo"`, ya que solo se capitalizó la primera letra.

También podemos utilizar `String.capitalize/2` para especificar un conjunto de caracteres diferentes como capitalización. Por ejemplo:

```elixir
String.capitalize("hola mundo", ["o"])
```

En este caso, solo la letra "o" será capitalizada, produciendo la salida `"hOla mundO"`.

## Profundizando

Si queremos tener un mayor control sobre cómo se capitaliza una cadena, podemos utilizar `String.titlecase/1`, que acepta una cadena y devuelve una versión capitalizada con cada palabra en mayúscula, excepto las palabras especiales como "a", "el", "y" que permanecen en minúsculas.

Otra función útil es `String.upcase/1`, que convierte todas las letras de una cadena en mayúsculas. Por ejemplo:

```elixir
String.upcase("hola mundo")
```
produciendo la salida `"HOLA MUNDO"`.

## Ver También

- [Elixir String Functions](https://hexdocs.pm/elixir/String.html)
- [Mastering Elixir Strings](https://blog.appsignal.com/2018/06/12/elixir-alchemy-mastering-elixir-strings.html)
- [Elixir Tutorial: Basic Operations with Strings](https://www.freecodecamp.org/news/elixir-tutorial-basic-operations-with-strings-9d0609eae20b/)