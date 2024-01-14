---
title:                "Gleam: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å slette bestemte tegn fra en streng i Gleam-programmeringsspråket. Dette kan være for å fjerne unødvendige mellomrom, spesielle tegn eller annen uønsket tekst. I denne artikkelen vil vi utforske hvordan man kan gjøre dette, og hvorfor det kan være nyttig.

## Hvordan

Det første trinnet for å slette tegn som matcher et mønster i Gleam er å importere biblioteket `gleam/glob` ved å legge til følgende linje øverst i filen:

```Gleam
import gleam/glob
```

Deretter kan vi bruke funksjonen `delete_chars` for å slette tegn som matcher et mønster fra en gitt streng. Vi kan også bruke `match`-operatøren for å spesifisere et mønster å slette. Her er et eksempel på hvordan det kan se ut:

```Gleam
let str = "Dette er en test"
let slettet_str = glob.delete_chars(str, match {"er"})
```

Output vil være: "Dtt e n tst"

I dette tilfellet ble alle forekomster av "er" i strengen slettet.

Vi kan også bruke `match`-operatøren for å spesifisere et bredere mønster. For eksempel, hvis vi vil fjerne alle mellomrom i en streng, kan vi bruke følgende kode:

```Gleam
let str = "Dette er en test"
let slettet_str = glob.delete_chars(str, match {" "})
```

Output vil være: "Detteerentest"

## Dypdykk

`delete_chars`-funksjonen tar også et tredje argument som er et dodra-meningsverdi, det vil si en verdi som bare er en referanse til seg selv og ikke kan endres. Dette gjør det mulig å lage mer avanserte mønstre å slette. For eksempel, hvis vi ønsker å slette alle tall fra en streng, kan vi bruke følgende kode:

```Gleam
let numbers = "123456789"
let slettet_numbers = glob.delete_chars(numbers, match {glob.dodra{"0".."9"}})
```

Output vil være en tom streng, siden alle tallene er slettet.

Vær oppmerksom på at denne metoden bare sletter tegnene fra den opprinnelige strengen og returnerer en kopi. Den opprinnelige strengen vil ikke bli endret.

## Se også

- Offisiell Gleam dokumentasjon for `gleam/glob`: https://gleam.run/libraries/glob
- Gleam glob bibliotekets GitHub-side: https://github.com/gleam-lang/glob
- Andre nyttige Gleam biblioteker: https://github.com/gleam-lang/awesome-gleam#libraries