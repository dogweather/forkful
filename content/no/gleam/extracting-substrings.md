---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å trekke ut delstrenger er en operasjon hvor vi lager en ny streng fra en del av en eksisterende streng. Dette er nyttig for å manipulere og analyser data inni strenger.

## Hvordan:

Her er hvordan du trekker ut understrenger i Gleam:

```Gleam
import gleam/substring.{slice}

fn main() {
  let string = "Hallo, Verden!"

  let hello = slice(string, 0, 5)
  let world = slice(string, 7, 13)

  println(hello)  // "Hallo"
  println(world)  // "Verden!"
}
```
## Dypdykk:

Substring ekstraksjon er en vanlig streng-operasjon siden tidlige programmeringsspråk som C. Alternativer til `slice` funksjonen inkluderer `substring` og `substr` i andre språk, men de kan ha litt forskjellige parametere og oppførsel. `Slice` i Gleam bruker null-indeksering og 'start' og 'end' parameterne for å bestemme hvilken del av streng å beholde.

## Se også:

For flere detaljer og relaterte emner, sjekk ut disse kildene:

- Gleam's String modul (https://hexdocs.pm/gleam/gleam/string.html)
- Gleam's Substring modul (https://hexdocs.pm/gleam/gleam/substring.html)
- W3Schools String Methods (https://www.w3schools.com/js/js_string_methods.asp)