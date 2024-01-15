---
title:                "Extrayendo subcadenas"
html_title:           "Elixir: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas puede ser una tarea útil en la programación cuando se trabaja con grandes cantidades de datos y se desea acceder a partes específicas de una cadena de texto.

## Cómo hacerlo

```Elixir
# Definir una cadena de texto
cadena = "Este es un ejemplo de una cadena de texto."

# Extraer una subcadena utilizando índices de posición
subcadena = String.slice(cadena, 5..15)

# Imprimir la subcadena
IO.puts subcadena

# Salida: es un ejemplo
```

## Profundizando

La función `String.slice/2` se utiliza para extraer una subcadena de una cadena de texto, especificando los índices de posición inicial y final. Esto nos permite seleccionar fácilmente partes específicas de una cadena sin tener que recorrerla por completo.

Además, también se pueden utilizar patrones en lugar de índices de posición para extraer subcadenas basándose en ciertas condiciones, lo que hace que esta función sea aún más versátil y poderosa.

## Ver también

- Documentación oficial de Elixir sobre `String.slice/2`: https://hexdocs.pm/elixir/String.html#slice/3
- Un tutorial sobre manipulación de subcadenas en Elixir: https://www.freecodecamp.org/news/substring-in-elixir-how-to-manipulate-strings-in-elixir-2adbfe126eff/