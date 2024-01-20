---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
I Gleam kan det å "kapitalisere" en streng bety dette å gjøre det første tegnet i ordet stort, som fra 'norge' til 'Norge'. Vi gjør dette for leselighet, formalitet, eller for å følge visse kodestandarder.

## How to:
```
import gleam/string

pub fn main() {
  let text = "oslo er kult"
  let capitalized_text = string.capitalize_first(text)
  io.println(capitalized_text)
}
```

Forventet utskrift:
```
Oslo er kult
```

## Deep Dive
Å kapitalisere strenger har røtter i typografien der stor forbokstav markerer starten på en ny setning eller viktig ord. I programmering har dette blitt en stil- og stavekonvensjon, særlig i brukergrensesnitt.

For alternativer kan man se på biblioteker som tilbyr mer funksjonalitet, som komplett store bokstaver (uppercase) eller stilformat som "Proper Case". Implementeringsdetaljene i Gleam bruker den underliggende Rust-implementasjon som gir god ytelse og Unicode-støtte.

## See Also
- [Unicode standard for case operations](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Rust's `char` method docs (used by Gleam under the hood)](https://doc.rust-lang.org/std/primitive.char.html#method.to_uppercase)