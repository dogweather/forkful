---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:14:45.560251-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Kellonaika ja päiväys, me kaikki tiedämme ne. Ohjelmointimaailmassa niitä tarvitaan lokien kirjoittamiseen, aikaleimojen luontiin ja moniin muihin. Sen tietäminen on työkalu, jonka haluat vyöllesi.

## How to:
Miten:

```Haskell
import Data.Time

-- Näin saat nykyisen päiväyksen
main :: IO ()
main = do
  currentDay <- getCurrentTime
  print $ utctDay currentDay
```

Tulostus voisi näyttää tältä:

```
2023-03-15
```

## Deep Dive
Syvä sukellus: `Data.Time` on moduuli, joka on osa `time`-kirjastoa. Se julkaistiin osana GHC:n standardikirjastoa aika päiviä sitten. Vaihtoehtoja? Vanhemmissa ohjelmissa on käytetty `old-time`-kirjastoa, mutta nykyaikana `time` on se juttu. Implementaatio? `getCurrentTime` hakee UTC-aikaleiman järjestelmästä. 

## See Also
Katso myös:

- Haskell `time` library documentation: [https://hackage.haskell.org/package/time/docs/Data-Time.html](https://hackage.haskell.org/package/time/docs/Data-Time.html)
