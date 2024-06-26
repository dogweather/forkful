---
date: 2024-01-20 17:50:51.573034-07:00
description: "C\xF3mo hacerlo: Elixir, un lenguaje construido sobre la Erlang VM (BEAM),\
  \ ha simplificado la vida de los programadores con su elegante interpolaci\xF3n\
  \ de\u2026"
lastmod: '2024-04-05T22:51:12.475577-06:00'
model: gpt-4-1106-preview
summary: "Elixir, un lenguaje construido sobre la Erlang VM (BEAM), ha simplificado\
  \ la vida de los programadores con su elegante interpolaci\xF3n de cadenas desde\
  \ su primera versi\xF3n lanzada en 2011."
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

## Cómo hacerlo:
```elixir
nombre = "Mundo"
saludo = "Hola, #{nombre}"
IO.puts saludo
```
Salida:
```
Hola, Mundo
```

Otro ejemplo con expresiones:
```elixir
precio = 100
mensaje = "El costo total es #{precio * 1.16} después de impuestos"
IO.puts mensaje
```
Salida:
```
El costo total es 116.0 después de impuestos
```

## Análisis Profundo
Elixir, un lenguaje construido sobre la Erlang VM (BEAM), ha simplificado la vida de los programadores con su elegante interpolación de cadenas desde su primera versión lanzada en 2011. A diferencia de otros lenguajes como Python, donde puedes usar el método `format` o f-strings, en Elixir simplemente pones `#{}` y la expresión dentro. Esta notación es tomada directamente de Ruby, influencia reconocida por José Valim, el creador de Elixir. 

Esta simplicidad no compromete la potencia: dentro de `#{}` puedes poner cualquier expresión válida de Elixir, incluyendo llamadas a funciones. Esto hace que la interpolación sea increíblemente flexible. Además, cuando Elixir compila el código, convierte las cadenas interpoladas en concatenaciones cuando es necesario, optimizando el rendimiento sin esfuerzo por parte del programador.

Aparte de la interpolación, Elixir ofrece otras formas de manipulación de cadenas como concatenación usando el operador `<>`, pero la interpolación es preferida por su claridad y brevedad cuando se trata de incrustar variables o expresiones dentro de una cadena.

## Ver También
- Documentación oficial sobre cadenas en Elixir: [Elixir String Docs](https://hexdocs.pm/elixir/String.html)
- Erlang y su relación con Elixir: [Erlang and Elixir](https://elixir-lang.org/crash-course.html#erlang)
- Entrevistas y charlas con José Valim, creador de Elixir: [José Valim Talks](https://www.youtube.com/results?search_query=jose+valim)
