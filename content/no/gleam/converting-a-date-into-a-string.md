---
title:                "Konvertere en dato til en streng"
html_title:           "Gleam: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng er en vanlig oppgave for programmerere. Dette betyr rett og slett å ta en dato-verdi og gjøre den om til en tekst-streng. Dette kan være nyttig for å presentere datoer på en mer lesbar måte, eller for å sammenligne og sortere dato-verdier.

## Slik gjør du det:
Gleam har innebygd støtte for å konvertere datoer til strenger ved hjelp av funksjonen `Date.to_string()`. Her er et eksempel som viser hvordan du bruker denne funksjonen og hvilket resultat du kan forvente:

```Gleam
// Definerer en dato-variabel:
let my_date = Date.new(2021,1,1)

// Konverterer til streng og lagrer i en ny variabel:
let my_string_date = Date.to_string(my_date)

// Skriver ut string-variabelen:
Debug.todo(my_string_date)

// Output: "2021-01-01T00:00:00.000000Z"
```

Som du kan se blir datoen konvertert til en streng som følger ISO 8601-standarden, som er et vanlig format for å presentere datoer og tidspunkter.

## Dykk dypere:
Det å konvertere en dato til en streng er ikke en spesielt avansert oppgave, men det kan likevel være lurt å vite hvordan denne funksjonen fungerer og hvorfor det kan være nyttig. I eldre programmeringsspråk måtte man ofte bruke spesifikke funksjoner for å håndtere datoer og tidspunkter, mens Gleam gjør dette mye enklere med en dedikert `Date`-type og innebygd støtte for konverteringer. Det finnes også andre muligheter for å representere og arbeide med datoer og tidspunkter, som for eksempel å lagre det som tall eller bruke ulike formateringsmetoder. Utforsk gjerne flere alternativer og finn ut hva som fungerer best for ditt spesifikke prosjekt.

## Se også:
- Offisiell Gleam dokumentasjon om datatyper og konverteringer: https://gleam.run/documentation/datatypes
- En god oversikt over hvordan datoer og tidspunkter håndteres i ulike programmeringsspråk: https://en.wikipedia.org/wiki/Date_format_by_country