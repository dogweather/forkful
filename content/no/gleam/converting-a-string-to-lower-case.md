---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:38:19.549934-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver innebærer å endre alle tekstens store bokstaver til små bokstaver. Programmerere gjør dette for å standardisere tekstdata, noe som er nyttig for søk og sammenligninger.

## Slik gjør du:
I Gleam kan du bruke `String.to_lower`-funksjonen for å konvertere en streng til kun små bokstaver.
```gleam
import gleam/io
import gleam/string

fn main() {
  let message = "Hei, Norge!"
  let lower_case_message = string.to_lower(message)
  io.println(lower_case_message)
}
```
Output:
```
hei, norge!
```

## Dypdykk
Historisk sett har behandling av tekster i varierende bokstavstørrelser vært en innviklet sak. I mange språk og programmeringsverktøy har funksjonen for å konvertere strenger til små bokstaver vokst fram for å forenkle tekstmanipulering og -analyse. For eksempel gjør småfying tekst mindre følsom for casesensitivitet under sammenligninger. Dette er nyttig ettersom "Eksempel" og "eksempel" i et case-insensitivt miljø anses som like. Alternativer til `String.to_lower` kan innbefatte å kjøre egendefinert kode som manuelt endrer hver bokstav ved hjelp av ASCII- eller Unicode-verdier, men dette er mer komplekst og feilutsatt. Implementasjonen til `String.to_lower` tar høyde for lokalisering og språkspecifikke regler, noe som er kritisk i moderne globaliserte applikasjoner.

## Se Også
- Gleam documentation on Strings: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Unicode Case Mapping Info: [http://www.unicode.org/versions/Unicode13.0.0/ch03.pdf](http://www.unicode.org/versions/Unicode13.0.0/ch03.pdf) (Unicode 13.0 Reference)
