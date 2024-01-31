---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:14:54.489295-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Hente datoen akkurat nå lar deg se og bruke dagens dato i programmet ditt. Det er nyttig for logging, tidsstemplinger eller funksjoner som avhenger av den reelle datoen.

## How to:
For å hente dagens dato i Haskell bruker du `Data.Time` biblioteket. Slik ser enkel kode ut:

```Haskell
import Data.Time

main :: IO ()
main = do
    currentDay <- getCurrentTime
    putStrLn $ "Dagens dato er: " ++ show (utctDay currentDay)
```

Kjører du dette, vil det se slik ut i terminalen:

```
Dagens dato er: 2023-04-12
```

Husk at `getCurrentTime` gir deg tiden i UTC.

## Deep Dive
Før `Data.Time` ble standard, burde du kanskje brukt eldre biblioteker som `old-time`, men nå antar vi `Data.Time` for å være veien å gå. 

`Data.Time` er robust og tar hensyn til tidssoner, skuddsekunder og andre tidsrelaterte nøttene. Du kan også manipulere datoer og tider med funksjoner som `addDays` eller `diffUTCTime`.

Imidlertid, hvis du trenger enda mer kontroll, som å jobbe med ulike kalendere, kan du utforske biblioteker som `time-recurrence` eller `chronos`. 

Som programmerer i Haskell, har du makt til å velge det biblioteket som passer best for din applikasjon når det gjelder å jobbe med dato og tid.

## See Also
Her er noen kilder for videre lesing og dypere forståelse:
- `Data.Time` dokumentasjon: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Om tidssoner: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
- Ta en titt på alternative biblioteker: https://hackage.haskell.org/package/chronos
- Haskell's egen guide til dato og tid: https://www.haskell.org/haskellwiki/Working_with_time
