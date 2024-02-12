---
title:                "Interpolación de cadenas de texto"
aliases:
- /es/elixir/interpolating-a-string.md
date:                  2024-01-20T17:50:51.573034-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La interpolación de cadenas en programación implica insertar dinámicamente valores dentro de una cadena de texto. Los programadores la utilizan para construir mensajes personalizados, combinar datos con texto, o para la manipulación de cadenas en general, haciéndolo de forma legible y eficiente.

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
