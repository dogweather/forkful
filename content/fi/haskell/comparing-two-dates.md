---
title:                "Kahden päivämäärän vertailu"
html_title:           "Haskell: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen vertailla kahta päivämäärää, esimerkiksi tarkasteltaessa tapahtumien järjestystä tai laskiessa ajanjaksojen pituuksia. Haskellin avulla tämä onnistuu helposti ja tarkasti.

## Miten tehdä

Vertailemalla päivämääriä käytetään `compare` -funktiota, joka palauttaa järjestysnumeron kahdelle annetulle arvolle. Voimme hyödyntää myös `Day` -tyyppiä, joka mahdollistaa päivämäärän esittämisen.

```Haskell
import Data.Time

-- Päivämäärät
date1 :: Day
date1 = fromGregorian 2020 2 20 -- 20.2.2020

date2 :: Day
date2 = fromGregorian 2020 3 10 -- 10.3.2020

-- Funktio päivämäärien vertailuun
compareDates :: Day -> Day -> Ordering
compareDates = compare

-- Vertaillaan date1 ja date2
compareDates date1 date2 -- Palauttaa "LT" eli "Less Than"
```

Tuloksena saadaan `Ordering` -tyyppinen arvo, joka voi olla `LT` (Less Than), `GT` (Greater Than) tai `EQ` (Equal). Vaihtoehtoisesti voimme käyttää myös `diffDays` -funktiota, joka laskee päivien määrän kahden päivämäärän välillä.

```Haskell
-- Funktion diffDays käyttö
diffDays date1 date2 -- Palauttaa -18 päivää
```

## Syvemmälle

Haskellissa päivämääriä käsittelemiseen on tarjolla myös muita hyödyllisiä työkaluja, kuten `UTCTime` ja `ZonedTime` -tyypit. Lisäksi `Data.Time.Calendar` -kirjastossa on monipuolisia toimintoja päivämäärien käsittelyyn.

## Katso myös

- [Haskellin virallinen dokumentaatio päivämäärien käsittelyyn](https://www.haskell.org/onlinereport/standard-prelude.html#type-class-Ord)
- [Päivämäärien vertailun opetusohjelma](https://wiki.haskell.org/Date_and_time_library)