---
title:                "Interpolazione di una stringa"
date:                  2024-01-20T17:50:57.356598-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
L'interpolazione di stringhe permette di inserire variabili o espressioni all'interno di una stringa di testo. Programmatori lo fanno per creare messaggi dinamici e migliorare la leggibilità del codice.

## How to (Come fare):
In Gleam, l'interpolazione di stringa si fa con la sintassi `#{}`:
```gleam
fn main() {
  let name = "Mondo"
  let greeting = "Ciao #{name}!"
  io.println(greeting)
}
```
Output:
```
Ciao Mondo!
```

## Deep Dive (Approfondimento)
L'interpolazione di stringhe non è una novità, risale a linguaggi più anziani come Perl e Ruby. In Gleam, il meccanismo è diretto e chiaro grazie alla sintassi espressiva. Anziché concatenare stringhe con `++`, `${name}` è più leggibile e previene errori. Gleam, essendo un linguaggio fortemente tipizzato, controlla anche che i valori interpolati siano compatibili con la stringa, aumentando la sicurezza del codice.

## See Also (Vedi Anche)
- [Gleam's official string interpolation documentation](https://gleam.run/book/tour/strings.html)
- [A comparison of string interpolation in various programming languages](https://en.wikipedia.org/wiki/String_interpolation#In_programming_languages)