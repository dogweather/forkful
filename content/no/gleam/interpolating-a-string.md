---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:50:46.899659-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
String-interpolasjon lar deg sette verdier inn i tekststrenger. Det sparer tid og gjør koden mer leselig ved å flette variabler og tekst sammen på en retningslinje.

## Slik gjør du:
```gleam
fn main() {
  let name = "Olav"
  let greeting = "Hei, \(name)!"
  io.println(greeting)
}

// Utdata: Hei, Olav!
```

## Dypdykk
Strings har blitt manipulert siden programmeringens barndom. I gamle språk kunne det være knot, men moderne språk som Gleam gjør det enkelt og effektivt. Alternativer til interpolasjon inkluderer manuell sammenslåing av strenger eller bruk av formateringsfunksjoner. Implementeringen skjer ofte ved at kompilatoren eller tolken erstatter variabelreferansen med dens verdi direkte i den endelige strengen.

## Se også
- Gleam language documentation: [https://gleam.run/book](https://gleam.run/book)
- String manipulation best practices: [https://pragprog.com](https://pragprog.com)