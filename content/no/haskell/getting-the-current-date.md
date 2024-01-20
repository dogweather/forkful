---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å få dagens dato i programmering er prosessen med å hente den nåværende datoen fra systemet. Dette gjøres ofte for å tidstemple transaksjoner, logger, eller for å utføre funksjoner avhengig av tid.

## Slik gjør du:

Å få dagens dato i Haskell er enkelt ved hjelp av `Data.Time`-biblioteket. Her er et eksempel på hvordan du kan gjøre det.

```Haskell
import Data.Time

main = do
    nå <- getCurrentTime
    putStr ("Dagens dato er: " ++ (show nå))
```
Dette eksemplet henter dagens dato og tid og skriver den ut til konsollen. 

## Dypdykk:

Haskell-biblioteket `Data.Time` ble introdusert i Haskell-plattformen 2010.2.0.0 og har vært en integrert del av standardbiblioteket siden.

Alternativt kan du bruke `System.Time` til å hente tiden, men det ansees som eldre og er mer feilutsatt enn `Data.Time`.

Få den nåværende tiden i Haskell utføres ved å kalle funksjonen `getCurrentTime` som returnerer en `IO UTCTime`. Dette betyr at du faktisk utfører noe side-påvirkning (IO), som å lese fra systemklokken.

## Se også:

For mer informasjon om `Data.Time`, sjekk ut det offisielle dokumentasjonen: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html 

For detaljert informasjon om tidsstyring i Haskell, sjekk ut denne artikkelen: https://wiki.haskell.org/High-level_timestamp_handling.