---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qué y Por qué?

Extracción de subcadenas es el proceso de tomar una parte pequeña de una cadena más grande. Los programadores lo hacen para manipular y analizar datos de mejor manera.

## Cómo hacerlo:

Aquí hay algunos ejemplos de cómo extraer subcadenas en Elixir:

```Elixir
cadena = "Hola, Elixir."
IO.puts String.slice(cadena, 0, 4)  # Output: "Hola"

cadena2 = "Practicamos Elixir todos los días."
IO.puts String.slice(cadena2, 14, -1) # Output: "Elixir todos los días."
```

## Profundizando

A lo largo de la historia de la programación, la extracción de subcadenas ha sido una herramienta esencial en el bolsillo del programador. En el mundo de Elixir, no tienes que reinventar la rueda gracias a múltiples formas de poder realizar este proceso.

Mencionamos la función `String.slice/3` que es bastante popular. Sin embargo, también puedes usar la función `binary_part/3` en Elixir para obtener el mismo resultado.

```Elixir
cadena = "Hola, Elixir."
IO.puts :binary.part(cadena, {0, 4})  # Output: "Hola"

cadena2 = "Practicamos Elixir todos los días."
IO.puts :binary.part(cadena2, {14, String.length(cadena2) - 14}) # Output: "Elixir todos los días."
```
Es importante tener en cuenta que la función `binary_part/3` toma un rango en forma de tupla como su segundo argumento, y la longitud de la subcadena como su tercer argumento.

## Ver También

- [Documentación de Elixir: String](https://hexdocs.pm/elixir/String.html)
- [Documentación de Elixir: Binary](https://hexdocs.pm/elixir/Binary.html)