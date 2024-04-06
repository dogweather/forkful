---
date: 2024-01-20 17:47:06.021980-07:00
description: "C\xF3mo hacerlo: Salida."
lastmod: '2024-04-05T21:54:00.047758-06:00'
model: gpt-4-1106-preview
summary: ''
title: Calculando la longitud de una cadena
weight: 7
---

## Cómo hacerlo:
```elixir
cadena = "Hola, Mundo"
longitud = String.length(cadena)
IO.puts longitud
```
Salida:
```
11
```

## Profundización
En Elixir, la función `String.length/1` nos da la longitud de una cadena en tiempo constante. Esto se debe a que, bajo el capó, Elixir cuenta con una representación binaria UTF-8 para las cadenas, y mantiene la longitud actualizada. 

Históricamente, otros lenguajes como C requerían recorrer toda la cadena para contar sus caracteres, lo que llevaba más tiempo con cadenas más largas. Elixir, al ser un lenguaje moderno, optimiza esto.

Una alternativa para contar la longitud podría ser implementar un bucle que atraviese cada carácter, pero esto sería reinventar la rueda y sería más ineficiente que usar `String.length/1`.

Respecto a los detalles de implementación, es importante saber que `String.length/1` cuenta los puntos de código Unicode y no solo los bytes, lo que significa que tendrá en cuenta caracteres compuestos, como emoticonos o letras acentuadas.

## Véase También
- Documentación oficial de Elixir para la función `String.length/1`: https://hexdocs.pm/elixir/String.html#length/1
- Un curso introductorio de Elixir, incluyendo operaciones con cadenas: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
- Preguntas frecuentes sobre cadenas en Unicode: http://www.unicode.org/faq/strings.html
