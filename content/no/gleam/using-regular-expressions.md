---
title:                "Gleam: Å bruke regulære uttrykk"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Hvorfor

Hvorfor bry seg med regulære uttrykk (Regular Expressions)? Regulære uttrykk er et kraftig verktøy for å håndtere strenger av tekst og lar deg finne, erstatte og manipulere tekst på en enkel måte. De er spesielt nyttige når du jobber med store mengder tekst eller når du trenger å filtrere og sortere data.

##Slik gjør du det

Du kan bruke regulære uttrykk i Gleam ved å bruke `regex` biblioteket. La oss si at du har en liste over produktnumre som følger et bestemt mønster, for eksempel "GN-1234", "GN-5678", "BN-9012". Ved hjelp av regulære uttrykk kan du enkelt filtrere ut bare produktene som starter med "GN".

```Gleam
import regex

// Opprett et regulært uttrykk for å matche "GN" etterfulgt av 4 tall
let pattern = regex.compile("^GN-\\d{4}$")

// Opprett en liste av produktnummere
let product_numbers = ["GN-1234", "GN-5678", "BN-9012"]

// Fjern alle produktnummer som ikke passer med mønsteret
let filtered_numbers = 
    product_numbers
    |> List.filter(fn(number) -> regex.match(pattern, number))

// Resultatet blir ["GN-1234", "GN-5678"]
```

##Dypdykk

Regulære uttrykk kan virke kompliserte og forvirrende i starten, men når du skjønner hvordan de fungerer, vil du se hvor nyttige de kan være. Her er noen tips for å hjelpe deg med å komme i gang:

- `^` og `$` tegnene matcher begynnelsen og slutten av en streng.
- `.` matcher hvilket som helst tegn.
- `*` matcher 0 eller flere forekomster av det foregående uttrykket.
- `+` matcher 1 eller flere forekomster av det foregående uttrykket.
- `?` matcher 0 eller 1 forekomst av det foregående uttrykket.
- `|` lar deg definere flere alternativer til et uttrykk.

Det er bare noen få av mange nyttige tegn og kombinasjoner som kan brukes i regulære uttrykk. Husk at øvelse gjør mester, så fortsett å eksperimentere og utforsk mulighetene for å få mest mulig ut av dette verktøyet.

##Se også

- [Gleam sin offisielle dokumentasjon om regex](https://gleam.run/book/tour/pattern_matching.html)
- [En interaktiv tutorial for regulære uttrykk](https://regexone.com/)
- [En online regex editor for Gleam](https://regexr.com/?language=gleam)