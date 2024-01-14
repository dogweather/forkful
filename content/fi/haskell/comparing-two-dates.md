---
title:                "Haskell: Kahden päivämäärän vertailu"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertailu on tärkeää esimerkiksi ohjelmoinnissa, jossa tiedon käsittely tapahtuu usein päivämäärien avulla. Vertailu auttaa selvittämään, onko tietty päivämäärä ennen vai jälkeen toista ja siten ohjaamaan ohjelman toimintaa.

## Kuinka vertailla päivämääriä Haskellissa?

Haskellissa päivämäärät voidaan esittää util-paketin `Data.Time` moduulilla. Alla olevassa koodiesimerkissä vertaillaan kahta päivämäärää käyttäen `diffDays` funktiota, joka palauttaa päivien määrän kahden päivämäärän välillä.

```Haskell
import Data.Time

paivamaara1 :: Day
paivamaara1 = fromGregorian 2020 3 15 

paivamaara2 :: Day
paivamaara2 = fromGregorian 2020 3 20 

ero :: Integer
ero = diffDays paivamaara2 paivamaara1
```

Yllä olevassa esimerkissä `ero` muuttujaan tallennetaan päivien määrä, joka on kulunut `paivamaara1` ja `paivamaara2` välillä. Tämä lasketaan `diffDays` funktiolla, joka ottaa parametreiksi kaksi päivämäärää ja palauttaa niiden välisen päivien määrän kokonaislukuna.

## Syvempi sukellus päivämäärän vertailuun

Päivämäärien vertailua voidaan tehdä myös tarkemmin `Data.Time` moduulin avulla. Esimerkiksi `getGregorian` funktiolla voidaan selvittää päivämäärän vuosi, kuukausi ja päivä. Samoin `diffUTCTime` funktiolla voidaan laskea aikaero kahden päivämäärän välillä.

Päivämäärien vertailuun liittyvään täsmälliseen syvempään tietoon voi tutustua lisää `Data.Time` moduulin dokumentaatiosta.

## Katso myös

- [Data.Time moduulin dokumentaatio](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Päivämäärien vertailun erityiskäsitteet Haskellissa](https://stackoverflow.com/questions/10194289/comparing-two-hours-in-haskell)