---
title:                "Haskell: Päivämäärän laskeminen tulevaisuuteen tai menneeseen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneeseen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Miksi
Jokaisella on joskus tarve laskea tuleva tai mennevä päivämäärä tietyllä etäisyydellä nykyisestä päivästä. Tähän tarkoitukseen Haskell tarjoaa joukon tehokkaita työkaluja, joita voi käyttää helposti ja vaivattomasti.

## Kuinka
Laskeminen tulevaa tai mennevä päivämäärää Haskellilla on helppoa ja nopeaa. Alla on esimerkki koodista, joka käyttää `addDays` funktiota ja tulostaa päivämäärän 30 päivää nykyisen päivämäärän jälkeen:

```Haskell
import Data.Time

tulevaPaivamaara = addDays 30 $ fromGregorian 2021 12 1
```

Tulos:

`2021-12-31`

## Syvällinen sukellus
Haskellin `Data.Time` kirjasto tarjoaa monipuolisen valikoiman funktioita ja tietotyyppejä päivämäärä- ja aikamuunnoksiin. Esimerkiksi `diffDays` funktio laskee päivämäärätieteellisen etäisyyden kahden päivämäärän välillä. Alla olevassa esimerkissä käytämme `diffDays` funktiota laskemaan päivien määrän kahden päivämäärän välillä:

```Haskell
import Data.Time

nykyinenPaivamaara = fromGregorian 2021 1 1
tulevaPaivamaara = fromGregorian 2021 12 1

paivaero = diffDays tulevaPaivamaara nykyinenPaivamaara
```

Tulos:

`334`

## Nähdäksesi myös
- [Data.Time - Hackage](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Haskellin oppiminen - Opas aloittelijoille](https://www.haskell.org/learn/)
- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)