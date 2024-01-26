---
title:                "Uthenting av delstrenger"
date:                  2024-01-20T17:45:57.894286-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Utdrag av understrenger lar deg hente spesifikke deler av en tekststreng. Det er nyttig for å analysere data, manipulere tekst og når man jobber med brukerinput.

## How to:
Gleam gjør det enkelt med sin standardbibliotek-funksjonalitet for strenger. Her er noen eksempler:

```gleam
import gleam/string

// Start-index inkludert, slutt-index ekskludert
pub fn eksempel_en() {
  let tekst = "Heisann, Sveisann!"
  string.slice(tekst, 0, 7) // "Heisann"
}

// Negativ indexering støttes ikke, bruk lengde for beregning
pub fn eksempel_to() {
  let tekst = "Fjell og daler"
  let lengde = string.len(tekst) // 13
  string.slice(tekst, lengde-5, lengde) // "daler"
}

pub fn main() {
  eksempel_en() |> io.debug // Skriver ut "Heisann"
  eksempel_to() |> io.debug // Skriver ut "daler"
}
```

## Deep Dive
Utdraging av understrenger er en grunnleggende funksjon i programmering, tidlig implementert i språk som C og Perl. I eldre språk kunne det være mer komplekst, med manuell håndtering av minne og pekere. Gleam forbedrer brukeropplevelsen ved å ha sikre og enkle strengfunksjoner.

Strings i Gleam er Unicode-kompatible, noe som betyr at du kan forvente at tekst som inneholder emoji eller andre ikke-latinske tegn håndteres riktig. Dette er forskjellig fra noen eldre språk der Unicode ofte var et ettertanke.

Gleam bygger på de sterke typene og mønstergjenkjenningsfunksjonene fra Erlang-systemet, men legger til en mer moderne og brukervennlig syntaks. Dette sikrer både ytelse og pålitelighet.

## See Also
- Gleam's string module documentation: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Unicode string handling in programming: https://unicode.org/reports/tr18/
- Erlang's influence on Gleam: https://gleam.run/book/tour/erlang-ecosystem/
