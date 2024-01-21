---
title:                "Concatenación de cadenas de texto"
date:                  2024-01-20T17:34:27.949878-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Concatenar cadenas significa unir dos o más strings en uno solo. Los programadores hacen esto para construir mensajes, combinar datos, y formatear salidas de forma flexible.

## Cómo hacerlo:
```elixir
# Concatenación simple con el operador <>
nombre = "Juan"
saludo = "Hola, " <> nombre <> "!"
IO.puts saludo # Salida: Hola, Juan!

# Usando IO.inspect para mostrar el resultado directamente
IO.inspect "Elixir" <> " es " <> "increíble!" # Salida: "Elixir es increíble!"

# Concatenación dentro de una función
defmodule Saludador do
  def saluda(nombre), do: "Hola, " <> nombre <> ". ¿Cómo estás?"
end

IO.puts Saludador.saluda("Marta") # Salida: Hola, Marta. ¿Cómo estás?
```

## Detalles Profundos:
La concatenación de cadenas es un concepto tan antiguo como la programación misma. En Elixir, se realiza con el operador `<>`, eficiente y simple. A diferencia de lenguajes como Python, donde puedes usar `+` para concatenar, Elixir utiliza `<>` para ser explícito en que la operación es específica de cadenas de texto. Alternativamente, podemos utilizar funciones como `String.concat/2` o trabajar con listas de IO (listas de caracteres) para optimizar la concatenación en ciertos casos.

Elixir maneja strings como binarios UTF-8, lo que significa que es eficiente para caracteres Unicode pero requiere que los programadores entiendan la diferencia entre bytes y puntos de código si se trabaja con ciertos textos internacionales.

## Ver También:
- Documentación oficial de Elixir sobre Strings: https://hexdocs.pm/elixir/String.html
- Guía de inicio rápido de Elixir: https://elixir-lang.org/getting-started/basic-types.html#strings
- Para un manejo más avanzado de strings, la biblioteca Elixir `String` ofrece muchas funciones útiles: https://hexdocs.pm/elixir/String.html