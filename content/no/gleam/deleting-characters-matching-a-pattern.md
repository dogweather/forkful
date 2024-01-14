---
title:    "Gleam: Sletting av tegn som samsvarer med et mønster"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor

Å slette tegn som matcher et mønster kan være viktig for å lage effektive og ryddige programmer. Ved å fjerne unødvendige tegn, kan man forbedre ytelsen til programmet og gjøre koden enklere å lese og forstå.

## Hvordan

For å slette tegn som matcher et mønster i Gleam, kan man bruke funksjonen `String.replace` sammen med et regex-uttrykk. Her er et eksempel på hvordan dette kan gjøres:

```Gleam
import Gleam.Regex.String

let input = "Å slette tegn som matcher et mønster kan være viktig"
let pattern = Regex.compile("e+")
let output = String.replace(input, pattern, "")

assert output == "Å sltte tgn som matcher t mønstr kan vr viktg"
```

I koden over har vi først importert funksjonaliteten for regex-uttrykk fra Gleam-biblioteket. Deretter har vi definert en input-tekst og et regex-uttrykk som leter etter én eller flere "e"-tegn. Ved å bruke `String.replace`, kan vi erstatte alle tegn som matcher mønsteret med et tomt streng. Til slutt bruker vi `assert`-funksjonen for å sikre at outputen er som forventet.

## Dypdykk

Bak koden i eksempelet over ligger det et kraftig regex-bibliotek som gjør det mulig å lage avanserte mønstre for å matche tegn. Det er verdt å utforske dette biblioteket nærmere og lære hvordan man kan bruke flere søkealternativer, grenser og grupperinger for å finne og erstatte spesifikke tegn i tekststrenger.

# Se også

- [Gleam Regex bibliotek](https://gleam.run/packages/greggreg/regex)
- [Eksempler på regex-mønstre](https://www.regular-expressions.info/examples.html)
- [Gleam dokumentasjon](https://gleam.run/documentation)