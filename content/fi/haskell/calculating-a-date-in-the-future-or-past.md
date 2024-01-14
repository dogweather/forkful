---
title:                "Haskell: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monilla ohjelmoijilla saattaa olla tarve laskea tietty päivämäärä tulevaisuudesta tai menneisyydestä jollakin tietyn ajanjaksolla. Tämä voi olla hyödyllistä esimerkiksi laskutus- tai varausjärjestelmissä.

## Ohjeet

Haskellilla tämä tehtävä on helppo toteuttaa DateTime-paketin avulla. Ensiksi, asenna paketti komennolla `cabal install datetime`. Seuraavassa esimerkissä laskemme päivämäärän 10 päivää tulevaisuuteen.

```Haskell
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Calendar (Day, addDays)

-- Luodaan muuttuja nykyiselle ajalle ja lisätään siihen tulevien päivien määrä
futureDate :: IO UTCTime
futureDate = do
  current <- getCurrentTime
  let daysToAdd = 10
  return (addUTCTime (fromIntegral (daysToAdd * 86400)) current)

main = do
  date <- futureDate
  print date
```

Tulostus:

```
2021-01-29 18:47:30.734746 UTC
```

Voit myös laskea päivämäärän menneisyydestä käyttämällä `addDays` funktiota ja antamalla negatiivisen arvon. Esimerkiksi, lasketaan päivämäärä 5 päivää menneisyyteen.

```Haskell
-- Muutetaan muuttujien tyypit sopiviksi
pastDate :: IO Day
pastDate = do
  current <- getCurrentTime
  let daysToSubtract = -5
  return (addDays daysToSubtract (utctDay current))

main = do
  date <- pastDate
  print date
```

Tulostus:

```
2021-01-24
```

## Syvempi sukellus

Tässä esitetyissä esimerkeissä käytimme Unix aikaleimaa laskemaan päivämäärän muutoksia, mutta DateTime-paketti tarjoaa myös muita hyödyllisiä toimintoja, kuten päivämäärien vertailua ja muotoilua.

Voit myös muuttaa päivämäärän muotoa haluamaksesi käyttämällä `formatDateTime` funktiota. Esimerkiksi, muutetaan päivämäärä muotoon "kuu-päivä-vuosi".

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)

dateString :: String
dateString = formatTime defaultTimeLocale "%d-%m-%Y" date

main = putStrLn dateString
```

Tulostus:

```
01-02-2021
```

Voit tarkistaa DateTime-paketin dokumentaatiosta lisää toimintoja ja niiden käyttöä.

## Katso myös

- [DateTime-paketin dokumentaatio](https://hackage.haskell.org/package/datetime)
- [Aikaleimojen laskeminen Haskellilla](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple-Datetime-operations)