---
title:                "Haskell: Sammenligne to datoer"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang hatt behov for å sammenligne to datoer i Haskell? Det kan være nyttig i en rekke situasjoner, som for eksempel å finne ut om en hendelse har skjedd før eller etter en annen, eller for å sortere en liste med datoer etter deres kronologiske orden. I denne artikkelen vil vi ta en titt på hvordan du kan sammenligne to datoer ved hjelp av Haskell.

## Hvordan

For å sammenligne to datoer i Haskell, må vi først importere `Data.Time` biblioteket. Dette biblioteket inneholder nyttige funksjoner for å håndtere og manipulere datoer og klokkeslett. La oss se på et enkelt eksempel:

```Haskell
import Data.Time (parseTimeM, defaultTimeLocale)

-- Definer to forskjellige datoer
dato1Str = "2021-03-10"
dato2Str = "2021-03-15"

-- Konverter strengene til datoobjekter
dato1 = parseTimeM True defaultTimeLocale "%Y-%m-%d" dato1Str :: Maybe Day
dato2 = parseTimeM True defaultTimeLocale "%Y-%m-%d" dato2Str :: Maybe Day

-- Sammenlign datoene
case (dato1, dato2) of
    (Just d1, Just d2) -> if d1 < d2
                             then putStrLn "Dato 1 er før Dato 2"
                             else putStrLn "Dato 1 er etter Dato 2"
    _ -> putStrLn "Kunne ikke konvertere datoene"
```

I dette eksempelet bruker vi `parseTimeM` funksjonen til å konvertere strengrepresentasjonen av datoene til `Day` objekter. Vi bruker også `defaultTimeLocale` for å definere formatet på datoene våre. Deretter sammenligner vi de to datoene ved hjelp av en enkel `if...else` uttalelse.

La oss se på et annet eksempel der vi sammenligne datoer basert på klokkeslett:

```Haskell
import Data.Time (parseTimeM, defaultTimeLocale)

-- Definer to forskjellige tidspunkter
tid1Str = "10:30"
tid2Str = "08:45"

-- Konverter strengene til tidspunktsobjekter
tid1 = parseTimeM True defaultTimeLocale "%H:%M" tid1Str :: Maybe TimeOfDay
tid2 = parseTimeM True defaultTimeLocale "%H:%M" tid2Str :: Maybe TimeOfDay

-- Sammenlign tidspunktene
case (tid1, tid2) of
    (Just t1, Just t2) -> if t1 > t2
                             then putStrLn "Tid 1 er etter Tid 2"
                             else putStrLn "Tid 1 er før Tid 2"
    _ -> putStrLn "Kunne ikke konvertere tidspunktene"
```

I dette eksempelet bruker vi `TimeOfDay` objekter og sammenligner dem ved hjelp av `>` og `<` operatorene.

## Dykk dypere

Hittil har vi bare sett på enkle eksempler på hvordan man kan sammenligne datoer i Haskell. Men det finnes også andre nyttige funksjoner og metoder for å sammenligne datoer og klokkeslett. Noen av disse inkluderer:

- `diffDays`: Denne funksjonen tar inn to datoer og returnerer antall dager mellom dem.
- `diffUTCTime`: Denne funksjonen tar inn to klokkeslett og returnerer antall sekunder mellom dem.
- `showGregorian`: Denne funksjonen tar inn et datoobjekt og returnerer den formatert som en åbenbar Gregoriansk dato.

Det er også verdt å merke seg at Haskell har streng typekontroll, så når du sammenligner to datoer eller klokkeslett, må de være av samme type for å kunne sammenligne dem.

## Se også

- [Dokumentasjon for Data.Time biblioteket](https://hackage.haskell.org/package/time)
- [En mer omfattende guide for å håndtere datoer i Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/lets-play-date-time/lets-play