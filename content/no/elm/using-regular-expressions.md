---
title:                "Elm: Å bruke regulære uttrykk"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry deg om å bruke regulære uttrykk i Elm? Vel, de kan være svært nyttige når du skal håndtere tekst og data på en effektiv måte. Regulære uttrykk gir deg en fleksibel måte å søke, filtrere og manipulere tekstbaserte data på.

## Hvordan

For å bruke regulære uttrykk i Elm, må du først importere `Regex` biblioteket. Deretter kan du bruke funksjoner som `Regex.find` og `Regex.replace` for å søke og erstatte tekst i en streng.

Her er et eksempel på å finne alle forekomster av et telefonnummer i en tekst og erstatte dem med "XXX-XXX-XXXX" ved hjelp av regulære uttrykk:

```elm
import Regex

pattern = Regex.fromString "\\([0-9]{3}\\) ?[0-9]{3}-[0-9]{4}"

text = "(123) 456-7890 is my phone number."

output = Regex.replace pattern (\_ -> "XXX-XXX-XXXX") text

-- output blir "XXX-XXX-XXXX is my phone number."
```

Som du kan se, hjelper regulære uttrykk deg med å finne spesifikke mønstre i en tekst og gjøre endringer basert på disse mønstrene. Dette kan være svært nyttig når du jobber med store tekstfiler eller når du trenger å utføre komplekse søk.

## Dypdykk

Regulære uttrykk har et eget språk som brukes til å definere mønstre. For eksempel vil `\\([0-9]{3}\\)` i eksempelet over finne alle forekomster av et telefonnummer i formatet "(123)". Her er noen viktige "jokertegn" du kan bruke i regulære uttrykk i Elm:

- `.` står for et hvilket som helst tegn
- `+` betyr "én eller flere forekomster av", for eksempel `[0-9]+` vil finne alle tall i en tekst
- `*` betyr "null eller flere forekomster av"
- `[]` definerer et sett av tegn, for eksempel `[aeiou]` vil finne alle vokaler
- `()` brukes til å gruppere mønstre og kan brukes sammen med `|` for å definere flere alternativer

Det er mye å lære om regulære uttrykk, men med litt praksis kan de bli et kraftig verktøy i din programmeringsverktøykasse.

## Se også

- [Elm dokumentasjon for Regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [RegExr - verktøy for å teste og eksperimentere med regulære uttrykk](https://regexr.com/)
- [Introduksjon til regulære uttrykk i Elm](https://dev.to/kristoffermh/regular-expressions-in-elm-6hh)