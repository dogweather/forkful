---
title:    "Elm: Få dagens dato"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor
Å få den nåværende datoen er en vanlig oppgave i Elm-programmering. Det kan være nyttig for å vise brukeren når informasjonen ble oppdatert eller for å planlegge når noe skal vises eller skje. 

## Slik gjør du det
For å få den nåværende datoen i Elm, kan du bruke funksjonen `currentDate` fra `Time` biblioteket. Først må du importere dette biblioteket ved å legge til følgende linje øverst i koden din:

```Elm
import Time
```

Deretter kan du bruke `currentDate` funksjonen som følger:

```Elm
Time.currentDate
```

Dette vil gi deg et `Date` objekt som inneholder informasjon om den nåværende datoen, inkludert dag, måned, år, timer, minutter og sekunder. Du kan også spesifisere en tidssone som parameter til `currentDate` funksjonen for å få datoen i en bestemt tidssone.

```Elm
Time.currentDate Time.utc
```

Kjører denne koden vil returnere den nåværende datoen i UTC-tidssone. 

## Dypdykk
Å få den nåværende datoen i Elm er enkelt, men det er viktig å være klar over hvordan Elm håndterer datoer. Elm har en innebygd `Date` type som er satt sammen av dag, måned, år, timer, minutter og sekunder. Denne typen er immutable, noe som betyr at den ikke kan endres. Dette sikrer at datoverdier alltid er konsistente og unngår mulige feil.

For å bruke datoen i et brukergrensesnitt, kan du bruke funksjonen `Date.toYearMonthDay` fra `Time` biblioteket til å hente ut år, måned og dag fra `Date` objektet. Dette gjør det lettere å vise datoen på ønsket format. 

```Elm
Date.toYearMonthDay myDate
```

Hvis du trenger å sammenligne datoer, kan du bruke funksjonen `Date.compare` fra `Time` biblioteket. Denne funksjonen sammenligner to `Date` objekter og returnerer en `LT` hvis den første datoen er mindre enn den andre, `EQ` hvis de er like, og `GT` hvis den første datoen er større enn den andre. Dette kan være nyttig for å sortere datoer eller for å sjekke om en dato kommer før eller etter en annen.

## Se også
- [Offisiell Elm dokumentasjon for Time biblioteket](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Weekly nyhetsbrev om håndtering av datoer i Elm](https://elmweekly.nl/articles/handling-dates-in-elm)
- [Elm Slides om Time biblioteket](https://slides.com/kevinemu/to-time-in-elm#/)