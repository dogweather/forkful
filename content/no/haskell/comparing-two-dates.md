---
title:                "Sammenligning av to datoer"
html_title:           "Haskell: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har arbeidet med datoer i programmering, har du sannsynligvis måttet sammenligne to datoer for å bestemme rekkefølgen eller hvor lang tid det er mellom dem. I denne artikkelen vil vi se på hvordan du kan sammenligne to datoer i Haskell og få nyttig informasjon om dem. 

## Hvordan gjøre det

Først må vi importere Data.Time biblioteket. Dette biblioteket lar oss jobbe med datoer og tid i Haskell. Her er et eksempel på hvordan du kan sammenligne to datoer:

```Haskell
import Data.Time

-- Oppretter to datoer
date1 = fromGregorian 2021 7 12
date2 = fromGregorian 2021 7 15

-- Sjekker om date1 er før date2
date1 < date2  -- Output: True

-- Finn antall dager mellom to datoer
diffDays date1 date2  -- Output: 3
```

Her har vi definert to datoer ved hjelp av funksjonen `fromGregorian` som tar inn et år, måned og dag. Deretter sammenligner vi de to datoene ved å bruke `<` operatøren, som returnerer `True` hvis `date1` er før `date2`. 

Vi kan også få informasjon om hvor lang tid det er mellom to datoer ved å bruke `diffDays` funksjonen, som returnerer antall dager mellom de to datoene. 

## Dypdykk

Når vi sammenligner to datoer, er det viktig å være klar over hvordan de er representert i Haskell. Datoer i Haskell er objekter av typen `Day` som består av år, måned og dag. Det er også verdt å merke seg at hvis vi prøver å sammenligne to datoer med forskjellige tidsenheter, som for eksempel å sammenligne en dato med en klokkeslett, vil Haskell gi oss en typefeil. 

En annen ting å være oppmerksom på er at datoene vi bruker i eksemplet over er basert på det gregorianske kalendersystemet. Hvis du trenger å jobbe med andre kalendersystemer, kan du bruke forskjellige funksjoner som er tilgjengelige i Data.Time biblioteket. 

Haskell tilbyr også andre nyttige funksjoner for å sammenligne datoer, som for eksempel `addDays`, som lar deg legge til et bestemt antall dager til en dato. Du kan utforske disse funksjonene og lære mer om hvordan du arbeider med datoer ved å lese dokumentasjonen for Data.Time biblioteket. 

## Se også

For mer informasjon om å arbeide med datoer og tid i Haskell, kan du sjekke ut følgende ressurser:

- [Data.Time dokumentasjon](https://hackage.haskell.org/package/time-1.10.0.2/docs/Data-Time.html)
- [Real World Haskell - Working with Time and Dates](http://book.realworldhaskell.org/read/using-parsec.html) 
- [Learn You a Haskell - A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads#monads-as-containers)