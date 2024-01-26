---
title:                "Søking og erstatting av tekst"
date:                  2024-01-20T17:57:48.892961-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søke- og erstattefunksjoner lar deg finne tekststrenger og bytte dem ut med noe annet. Programmerere bruker dette for å oppdatere kode, korrigere feil, eller massemanipulere data.

## Slik gjør du:
```gleam
import gleam/string

pub fn search_and_replace_example() {
  let text = "Hilsen fra Norge!"
  let new_text = string.replace(text, "Norge", "Gleam-verdenen")
  new_text
}

// Output
//"Hilsen fra Gleam-verdenen!"
```
Denne koden viser en enkel søk-og-erstatt handling i Gleam.

## Dykk dypere
I de tidligste dagene av programmering var tekstmanipulasjon en langt mer omstendelig prosess, ofte med lavnivå-språk. Alternativer til Gleam for søk og erstatt inkluderer `regex`-biblioteker og verktøy som `sed` i Unix-baserte systemer. Implementasjonsdetaljer varierer, men de fleste moderne språk bruker abstraksjoner slik at programmerere enkelt kan manipulere strenger uten å tenke på de underliggende datastrukturene.

## Se også
- Regular expressions (regex) guide: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- GNU sed manual: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
