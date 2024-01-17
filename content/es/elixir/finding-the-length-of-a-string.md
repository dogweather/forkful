---
title:                "Encontrando la longitud de una cadena"
html_title:           "Elixir: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En programación, encontrar la longitud de una cadena significa determinar la cantidad de caracteres que contiene dicha cadena. Los programadores suelen hacerlo para realizar ciertas operaciones o validaciones en sus programas.

## Cómo:

```elixir
IO.puts String.length("Hola mundo!")
```
Salida: 11

En el ejemplo anterior, utilizamos la función `String.length` en Elixir para encontrar la longitud de la cadena "Hola mundo!" y luego utilizamos la función `IO.puts` para imprimir el resultado en la consola.

## Profundizando:

### Contexto histórico:
La idea de encontrar la longitud de una cadena ha existido desde los primeros lenguajes de programación. Inicialmente, esto se hacía mediante operaciones matemáticas y algoritmos complejos, pero con el tiempo han surgido funciones específicas para facilitar esta tarea.

### Alternativas:
Existen varias alternativas para encontrar la longitud de una cadena en Elixir. Además de la función `String.length`, también se puede utilizar el operador `length` como en `IO.puts "Hola" | length`, o la función `byte_size` para obtener la cantidad de bytes que ocupa la cadena.

### Detalles de implementación:
En Elixir, las cadenas son representadas como listas de caracteres ASCII y la función `String.length` simplemente cuenta el número de elementos en la lista. Sin embargo, es importante tener en cuenta las diferencias entre caracteres ASCII y Unicode al trabajar con cadenas en Elixir.

## Ver también:

- [Documentación oficial de Elixir sobre cadenas](https://hexdocs.pm/elixir/String.html)
- [Artículo sobre cadenas en Elixir](https://hackernoon.com/strings-and-elixir-part-1-7f6930f4d330)
- [Explicación detallada de cómo funcionan las cadenas en Elixir](https://underjord.io/how-string-works-in-elixir.html)