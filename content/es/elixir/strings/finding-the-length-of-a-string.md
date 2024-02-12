---
title:                "Calculando la longitud de una cadena"
aliases:
- /es/elixir/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:06.021980-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando la longitud de una cadena"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Encontrar la longitud de una cadena es simplemente determinar cuántos caracteres contiene. Lo hacemos para validar entradas, manipular texto o simplemente para saber el tamaño de la información que estamos manejando.

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
