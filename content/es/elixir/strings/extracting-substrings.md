---
date: 2024-01-20 17:45:30.270259-07:00
description: "Extraer subcadenas es sacar partes espec\xEDficas de un string. Los\
  \ programadores hacemos esto para manipular y procesar datos de textos como entrada\
  \ o para\u2026"
lastmod: 2024-02-19 22:05:17.278726
model: gpt-4-1106-preview
summary: "Extraer subcadenas es sacar partes espec\xEDficas de un string. Los programadores\
  \ hacemos esto para manipular y procesar datos de textos como entrada o para\u2026"
title: "Extracci\xF3n de subcadenas"
---

{{< edit_this_page >}}

## Qué es y Por Qué?
Extraer subcadenas es sacar partes específicas de un string. Los programadores hacemos esto para manipular y procesar datos de textos como entrada o para validaciones/formato.

## Cómo Hacerlo:
```elixir
# Obtener substring con String.slice/3
cadena = "Hola, Universo Elixir"
substring = String.slice(cadena, 7, 8) # Empieza en el índice 7, toma 8 caracteres.
IO.puts substring
# Salida: Universo

# Utilizando ranges con String.slice/2
substring = String.slice(cadena, 7..14) # También puedes usar 7..-12
IO.puts substring
# Salida: Universo

# Obteniendo una sola letra
letra = String.at(cadena, 0) # Índice empieza en 0.
IO.puts letra
# Salida: H
```

## A Fondo:
Antes de Elixir, venían otros lenguajes como Ruby o Python, donde ya se extraían subcadenas, pero Elixir brinda un enfoque funcional y concurrente a esta operación. Las alternativas incluyen patrones de coincidencia (pattern matching) o uso de funciones como `String.split/2`. Internamente, Elixir maneja las subcadenas de manera eficiente, debido a su naturaleza inmutable y la representación binaria del texto.

```elixir
# Ejemplo de pattern matching para obtener substrings
<<"Hola, ", resto::binary>> = "Hola, Universo Elixir"
IO.puts resto
# Salida: Universo Elixir
```

## Ver También:
- [String slice en la documentación oficial de Elixir](https://hexdocs.pm/elixir/String.html#slice/3)
- [Guía de Programación de Elixir](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Expresiones Regulares en Elixir](https://hexdocs.pm/elixir/Regex.html)
