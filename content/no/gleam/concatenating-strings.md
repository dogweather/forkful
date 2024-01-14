---
title:                "Gleam: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor concatenate strenger i Gleam? Det er et veldig vanlig problem å måtte kombinere flere små strenger for å lage en større. Dette kan være nyttig for ting som å formatere tekst, lage navn og generelt manipulere data.

## Hvordan

Du kan enkelt concatenate strenger i Gleam ved å bruke "+" operatoren. La oss si at vi har to strenger "Hei" og "verden", og vi ønsker å kombinere dem for å få "Hei verden". Dette er hvordan det ser ut i Gleam:

```Gleam
let setning = "Hei" + "verden"
```

Det er viktig å merke seg at strenger i Gleam er immutabel, noe som betyr at de ikke kan endres etter at de er opprettet. Dette betyr at når vi concatenate to strenger, oppretter vi egentlig en helt ny streng.

Vi kan også concatenate flere strenger sammen ved å bruke flere "+" operatører. For eksempel, hvis vi ønsker å legge til "!" til "Hei verden", kan vi gjøre det på følgende måte:

```Gleam
let setning = "Hei" + "verden" + "!"
```

La oss se på et eksempel med faktiske data. Vi har en funksjon som tar inn et navn og returnerer en hilsen med navnet inkludert:

```Gleam
fn hilsen(navn) {
  let hilsen = "Hei " + navn + ", velkommen til Gleam-bloggen!"
  hilsen
}
```

Når vi kaller denne funksjonen med navnet "Per", vil vi få følgende output:

```
Hei Per, velkommen til Gleam-bloggen!
```

## Deep Dive

Når vi concatenate en streng i Gleam, hva skjer egentlig under overflaten? Når vi bruker "+" operatoren, bruker Gleam faktisk en funksjon kalt "add_strings" som tar inn to strenger som argumenter og returnerer en ny concatenert streng. Dette er en optimalisering som forbedrer ytelsen ved å unngå å lage unødvendige kopier av strenger.

Videre, hvis vi bruker "+" operatoren i en rekke sammenhenger, vil Gleam automatisk skrive om det til en eneste "add_strings" funksjonskall. Dette forenkler koden og gjør den mer effektiv.

## Se også

- [Offisiell Gleam dokumentasjon for å concatenate strenger](https://gleam.run/book/core/string.html#concatenation)
- [Gleam-community forum](https://gleam.discourse.group/) for å dele og spørre om kodeeksempler
- [Gleam på GitHub](https://github.com/gleam-lang/gleam) for å følge videre utvikling og delta i samfunnet