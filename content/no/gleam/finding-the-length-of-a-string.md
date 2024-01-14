---
title:                "Gleam: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng kan være en viktig og nyttig oppgave i programmering, spesielt når du jobber med tekstbehandling. Det kan hjelpe deg med å manipulere og analysere data, samt utføre bestemte handlinger basert på lengden til en streng.

## Hvordan

For å finne lengden til en streng ved hjelp av Gleam-programmeringsspråket, kan du følge disse enkle trinnene:

```
Gleam import intern as core

// Definer en streng
let streng = "Hei verden!"

// Bruk funksjonen core.String.length til å finne lengden til strengen
let lengde = core.String.length(streng)

// Skriv ut lengden
Gleam io.format("Lengden til strengen er {}", [lengde])
```

Konsollutgang: Lengden til strengen er 11

Som du kan se, bruker vi funksjonen `core.String.length` for å finne lengden til strengen `Hei verden!`. Denne funksjonen tar inn en streng som argument og returnerer lengden som et heltall.

## Dypdykk

I Gleam's `core` -modul finner vi flere funksjoner som kan være nyttige når du jobber med strenger. For eksempel har vi `core.String.count` -funksjonen som kan telle hvor mange ganger et bestemt tegn eller en streng dukker opp i en annen streng. Vi har også `core.String.slice` -funksjonen som kan hjelpe deg med å hente ut en del av en streng basert på et gitt start- og sluttpunkt.

For å lære mer om de ulike funksjonene som er tilgjengelige i `core` -modulen for å jobbe med strenger, kan du sjekke ut documentasjonen på Gleams offisielle nettside.

## Se også

- [Gleam Dokumentasjon](https://gleam.run/documentation/)
- [Offisiell Gleam-nettside](https://gleam.run/)
- [Github repository for Gleam](https://github.com/gleam-lang/gleam)