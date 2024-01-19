---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Eliminar caracteres que coincidan con un patrón en Elixir se refiere a la eliminación de caracteres específicos en una cadena que coinciden con un patrón definido. Los programadores lo hacen generalmente para limpiar y organizar los datos, y para facilitar su procesamiento y análisis.

## Cómo se hace:
Aquí hay un ejemplo de cómo puedes eliminar caracteres que coincidan con un patrón en Elixir:

```elixir
    #Ejemplo de código
    iex> String.replace("¡Hola, Mundo!", ",", "")
    "¡Hola Mundo!"
```

El fragmento de código anterior elimina las comas de la cadena.

## Inmersión profunda
Eliminar caracteres por patrones se originó en los lenguajes de programación funcional como Elixir para realizar operaciones de cadena más eficientes y precisas. Aunque hay formas alternativas de lograr el mismo resultado, la eliminación de caracteres por patrón es rápida y eficiente especialmente cuando se trabaja con grandes volúmenes de datos.

Al usar 'String.replace', Elixir busca el patrón de caracteres en la cadena, y cada vez que encuentra una coincidencia, elimina los caracteres correspondientes. Internamente, 'String.replace' utiliza la funcionalidad de la biblioteca de erlang, lo que la hace extremadamente rápida y eficiente.

## Ver también
Para más información, puedes consultar las fuentes relacionadas:

- Guía oficial de Elixir para la manipulación de cadenas: https://elixir-lang.org/getting-started/basic-types.html#strings

- Documentación sobre la función `String.replace`: https://hexdocs.pm/elixir/String.html#replace/3

- Entrada de blog sobre la manipulación de cadenas en Elixir: http://learningelixir.joekain.com/string-manipulation-in-elixir/