---
title:                "Haskell: Å få nåværende dato"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få dagens dato kan være nyttig av en rekke årsaker når du driver med Haskell-programmering. Det kan for eksempel være nyttig for å lage tidstempler eller for å holde orden på ulike datoer i et program.

## Hvordan

Nå skal vi se på hvordan du kan få dagens dato i Haskell. Det finnes flere forskjellige måter å gjøre dette på, men vi skal se på to av de vanligste måtene.

### Metode 1: Bruke Data.Time biblioteket

Først må du importere Data.Time biblioteket ved å skrive ```import Data.Time``` øverst i programmet ditt. Deretter kan du bruke funksjonen ```getCurrentTime``` for å få dagens dato og tid. Eksempelvis:

```Haskell
import Data.Time

getCurrentDate :: IO Day
getCurrentDate = do
    time <- getCurrentTime
    return $ utctDay time
```

Dette vil gi deg dagens dato i formatet ```Year-Month-Day```. Hvis du vil ha en mer spesifikk dato og tid, kan du bruke funksjonen ```parseTimeOrError``` og spesifisere ønsket format. For å få datoen i det engelske formatet ```Month/Day/Year```, kan du for eksempel gjøre følgende:

```Haskell
import Data.Time.Format
import System.Locale

getCurrentDate :: IO String
getCurrentDate = do
    time <- getCurrentTime
    let format = "%m/%d/%Y"
    return $ formatTime defaultTimeLocale format time
```

### Metode 2: Bruke System.Time biblioteket

En annen måte å få dagens dato på er ved å bruke System.Time biblioteket. Dette kan være nyttig hvis du ikke ønsker å bruke Data.Time biblioteket av forskjellige årsaker.

Først må du importere System.Time biblioteket ved å skrive ```import System.Time``` øverst i programmet ditt. Deretter kan du lage en funksjon som kaller på ```getClockTime``` og ```toUTCTime```, som i dette eksempelet:

```Haskell
import System.Time

getCurrentDate :: IO Day
getCurrentDate = do
    time <- getClockTime
    return $ toUTCTime time
```

Denne metoden vil også gi deg dagens dato i formatet ```Year-Month-Day```.

## Dypdykk

Det er mye mer du kan gjøre med å få dagens dato i Haskell, for eksempel konvertere datoen til et annet format eller regne ut forskjeller mellom ulike datoer. Hvis du vil lære mer om hvordan du kan jobbe med datoer i Haskell, kan du sjekke ut følgende linker:

- [Offisiell Haskell dokumentasjon om datoer og tid](https://www.haskell.org/documentation/)
- [Stack Overflow diskusjon om å få dagens dato i Haskell](https://stackoverflow.com/questions/23661251/how-to-get-the-current-date-in-haskell)
- [HaskellWiki side om å jobbe med datoer i Haskell](https://wiki.haskell.org/Working_with_Dates_and_Time)
- [Haskell for dummies - Dato og tid](https://en.wikibooks.org/wiki/Haskell/DateTime)

## Se også

- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell for Dummies](https://en.wikibooks.org/wiki/Haskell)
- [Stack Overflow diskusjon om Haskell-programmering](https://stackoverflow.com/questions/tagged/haskell)