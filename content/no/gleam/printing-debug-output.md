---
title:                "Skrive ut feilsøkingsdata"
date:                  2024-01-20T17:52:23.705912-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å skrive ut debug-informasjon betyr å vise midlertidige data i konsollen for å forstå hva koden gjør. Programmerere gjør dette for å finne og fikse feil lettere.

## How to: (Hvordan:)
```gleam
import gleam/io

pub fn main() {
  let debug_value = "Hei fra Gleam!"
  
  io.debug(debug_value) // Skriver ut debug-informasjon i konsollen
}

// Eksempel på output:
// => "Hei fra Gleam!"
```
## Deep Dive (Dypdykk)
Debug-utskrifter har eksistert lenge, helt siden de tidlige dagene av programmering. På de gode, gamle dager måtte programmerere observere maskinens lamper og brytere for å feilsøke! I Gleam og mange andre moderne språk gir `debug` funksjonen en enkel måte å logge verdier på uten å påvirke produksjonsloggene. Det er alternativer som loggbiblioteker for mer strukturert og nivåbasert logging, men for rask og skitten innsikt, er `io.debug` din venn.

## See Also (Se Også)
- Gleam's official documentation on IO: https://hexdocs.pm/gleam/gleam/io.html
- Effective Logging Practices in Programming: https://www.oreilly.com/library/view/effective-logging-practices/0680REX2E_GL/
- Debugging Strategies for Programmers: https://www.informit.com/articles/article.aspx?p=1998559