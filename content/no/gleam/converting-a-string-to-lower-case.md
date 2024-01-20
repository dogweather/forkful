---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Gleam programmering: Hvordan konvertere en streng til små bokstaver

## Hva & Hvorfor?
I programming, handler konvertering av en streng til små bokstaver om å endre alle tegnene i en streng til deres lowercase-versjoner. Dette gjør det enklere for programmerere å sammenligne strenger effektivt, uavhengig av inputformatet.

## Hvordan:
For å konvertere en streng til små bokstaver i Gleam trenger du følgende kode:

```gleam
import gleam/string

fn main() {
    let text = "Hei VERDEN!"
    let lowercase_text = string.lowercase(text)

    // Dette vil printe 'hei verden!'
    io.println(lowercase_text)
}
```

## Dypdykk
Historisk sett har konvertering til små bokstaver vært avgjørende for å løse mange utfordringer innen tekstbehandling. Det har hjulpet utviklere med å skrive mer robuste koder ved å normalisere streng-input, slik at to strenger kan sammenlignes uten hensyn til kapitalisering.

Alternative metoder for å oppnå dette i andre programmeringsspråk inkluderer `.toLowerCase()` i JavaScript og `.lower()` i Python.

Hvordan dette faktisk fungerer i Gleam er, funksjonen `string.lowercase` gjennomgår hver bokstav i strengen, og erstatter den med dens små bokstavversjon.

## Se ogsa
2. [Gleam String Moduler](https://hexdocs.pm/gleam_stdlib/gleam/string.html)
3. [Wikipedia: ASCII](https://no.wikipedia.org/wiki/ASCII)