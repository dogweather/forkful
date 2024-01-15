---
title:                "Konvertere en dato til en streng"
html_title:           "Haskell: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har jobbet med datoer i Haskell, har du sikkert støtt på behovet for å konvertere dem til en streng. Dette kan være nyttig for å vise datoer i et brukergrensesnitt eller for å lagre dem som en del av en streng i en database. I denne artikkelen vil vi se på hvordan du kan gjøre nettopp det.

## Hvordan
Det finnes flere måter å konvertere en dato til en streng i Haskell, men den enkleste og mest populære metoden bruker funksjonen `show`. Denne funksjonen tar en dato som input og returnerer en streng som representerer datoen. La oss se på et eksempel:

```Haskell
import Data.Time
import Data.Time.Format

today :: Day
today = fromGregorian 2021 08 10

-- Konverterer dagens dato til en streng
todayString :: String
todayString = show today

-- Output: "2021-08-10"
```

Som du kan se, returnerer `show`-funksjonen en streng på formatet "ÅÅÅÅ-MM-DD". Dette er standardformatet for datoer i Haskell, men du kan også bruke `formatTime`-funksjonen fra `Data.Time.Format`-modulen for å tilpasse formatet etter dine behov.

```Haskell
import Data.Time
import Data.Time.Format

today :: Day
today = fromGregorian 2021 08 10

-- Konverterer dagens dato til en streng på formatet DD/MM/ÅÅÅÅ
todayString :: String
todayString = formatTime defaultTimeLocale "%d/%m/%Y" today

-- Output: "10/08/2021"
```

Det er også mulig å konvertere datoer til andre formater som f.eks. "august 10, 2021" ved å bruke `formatTime`-funksjonen og spesifisere riktig formatstreng.

## Dype dykk
Under panseret bruker `show`-funksjonen `instance Show`-implementeringen av datatypen `Day` fra `Data.Time.Calendar`-modulen. Denne implementeringen konverterer datoen til en streng ved hjelp av funksjonen `showGregorian` som tar inn en datotypen `Day` og returnerer en streng på formatet "ÅÅÅÅ-MM-DD". Derfor vil `show`-funksjonen alltid returnere datoen i dette formatet.

Hvis du ønsker å tilpasse formatet for alle datoer i prosjektet ditt, kan du definere en egen instans av `Show`-klassen for datatypen `Day` og bruke `formatTime`-funksjonen der. Dette gjør det også enklere å endre formatet hvis du senere skulle ønske det.

## Se også
- [Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Data.Time.Format](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Data.Time.Calendar](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)