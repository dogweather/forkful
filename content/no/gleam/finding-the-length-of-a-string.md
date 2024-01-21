---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:47:17.209653-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å finne lengden på en streng betyr å telle antall tegn den inneholder. Programmører trenger denne infoen for å validere input, manipulere tekst, eller bare for å sjekke størrelsen på dataene.

## How to: (Slik gjør du:)
```gleam
// La oss finne lengden på en streng
fn main() {
  let greeting = "Hei, verden!"
  let length = string.len(greeting)
  io.debug(length)
}

// Output vil være: 12
```

## Deep Dive (Dypdykk)
I eldre programmeringsspråk var det ikke alltid like lett å hente strenglengder, og en måtte ofte løpe gjennom hver bokstav manuelt. I Gleam, som mange moderne språk, er dette en enkel operasjon. 

Alternativer for å måle strenglengder inkluderer bruk av loop eller andre innebygde funksjoner, men `string.len` er både raskere og mer lesbar. Implementasjonsmessig bruker `string.len` effektive maskinnivåoperasjoner i de fleste systemer for å gi umiddelbar tilgang til strenglengden.

## See Also (Se også)
- Gleam's official documentation on strings: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)