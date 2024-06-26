---
date: 2024-01-20 17:42:03.943203-07:00
description: "C\xF3mo hacerlo: La eliminaci\xF3n de caracteres que corresponden a\
  \ un patr\xF3n no es algo nuevo. Desde los tiempos de Unix y sus expresiones regulares,\
  \ esta\u2026"
lastmod: '2024-04-05T21:54:00.040517-06:00'
model: gpt-4-1106-preview
summary: "La eliminaci\xF3n de caracteres que corresponden a un patr\xF3n no es algo\
  \ nuevo."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
```elixir
# Ejemplo simplificado usando String.replace/3
string = "¡Hola, Mundo123!"
patron = ~r/[0-9]/
nueva_cadena = String.replace(string, patron, "")

IO.puts nueva_cadena
# Salida esperada:
# ¡Hola, Mundo!
```

```elixir
# Ejemplo usando funciones en la librería de Regex
import Regex
string = "Datos99 irrelevantes 11serán 222eliminados."
patron = ~r/\d+/
nueva_cadena = Regex.replace(patron, string, "")

IO.puts nueva_cadena
# Salida esperada:
# Datos irrelevantes serán eliminados.
```

## Análisis en Profundidad
La eliminación de caracteres que corresponden a un patrón no es algo nuevo. Desde los tiempos de Unix y sus expresiones regulares, esta operación ha sido esencial para el tratamiento de texto. Elixir, influenciado por Erlang y otros lenguajes de programación funcional, utiliza un módulo llamado `Regex`, que está construido encima de la librería PCRE (Perl Compatible Regular Expressions). Como alternativas, se puede usar `String` con funciones como `replace/3` o `replace/4` para manipulaciones más sencillas, o recurrir a la recursividad y las funciones de alto orden para soluciones más creativas y específicas.

Otra posibilidad es implementar una función mediante el patrón de diseño "pipeline" que Elixir promueve, utilizando varias funciones que transforman la data paso a paso. Esto no solo es elegante, sino que también hace que el código sea fácil de leer y mantener.

## Ver También
- [Documentación oficial de `String.replace/3`](https://hexdocs.pm/elixir/String.html#replace/3)
- [Documentación oficial de `Regex`](https://hexdocs.pm/elixir/Regex.html)
- [Uso de pipelines en Elixir](https://elixirschool.com/en/lessons/basics/pipe_operator/)
