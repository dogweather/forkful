---
title:                "Uniendo cadenas de texto"
html_title:           "Elixir: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Si estás familiarizado con la programación, probablemente hayas utilizado la concatenación de cadenas en algún momento. Esta técnica te permite unir varias cadenas de texto para formar una sola, lo que puede ser útil en muchas situaciones, como la creación de mensajes personalizados o la manipulación de datos.

## Cómo hacerlo
Para concatenar cadenas en Elixir, podemos utilizar el operador `<>`. Veamos un ejemplo sencillo:

```Elixir
"Greetings" <> " from" <> " Elixir!" # Output: Greetings from Elixir!
```

En el código anterior, utilizamos el operador `<>` para unir tres cadenas de texto y formar una sola. También es posible concatenar variables, como en el siguiente ejemplo:

```Elixir
name = "John"
"Greetings " <> name <> "!" # Output: Greetings John!
```

Además del operador `<>`, también podemos utilizar la función `String.concat/2` que toma dos parámetros y los concatena en una cadena. Veamos otro ejemplo:

```Elixir
String.concat(["Welcome", " to", " Elixir!"]) # Output: Welcome to Elixir!
```

## Profundizando
En Elixir, las cadenas de texto son representadas como listas de caracteres, lo que significa que el operador `<>` y la función `String.concat/2` en realidad están concatenando listas. Esta es una de las razones por las que Elixir es tan eficiente en la manipulación de cadenas.

También es importante tener en cuenta que cada vez que concatenamos una cadena en Elixir, en realidad estamos creando una nueva cadena en memoria. Esto puede ser un problema si estamos trabajando con grandes cantidades de datos, ya que puede afectar el rendimiento de nuestra aplicación. Una forma de evitar esto es utilizando la función `<<>>` que permite unir cadenas sin crear una nueva cadena cada vez.

## Ver también
- ["Elixir Strings" por Elixirschool](https://elixirschool.com/es/lessons/basics/strings/)
- ["String API" en la documentación de Elixir](https://hexdocs.pm/elixir/String.html)