---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärän ilmaisemista tekstimuodossa - esim. "2. tammikuuta 2022". Se on hyödyllistä, koska se tekee päivämäärästä ihmislukuisen ja sitä voidaan käyttää esimerkiksi logitiedostoissa tai käyttöliittymässä.

## Miten:

Ensin tuomme tarvittavat moduulit:

```Haskell
import Data.Time
```

Tässä on yksinkertainen esimerkki päivämäärän muuntamisesta merkkijonoksi:

```Haskell
muunnaPvm :: IO String
muunnaPvm = do
   päivämäärä <- getCurrentTime
   return (show päivämäärä)
```

Tämän koodiesimerkin tulostus voi olla jotain tämän kaltaista: "2022-01-02 15:45:50.1234 UTC"

## Sukellus syvyyksiin:

Historiallinen yhteys: Yleisessä käytössä oleva päivän ja ajan esitysmuoto merkkijonoissa määriteltiin alun perin ISO 8601 -standardissa.

Vaihtoehdot: Haskellin `Data.Time` -moduulin lisäksi myös muissa kielissä, kuten Python tai JavaScript, löytyy vastaavat funktionaalisuudet päivämäärien käsittelyyn.

Toteutuksen yksityiskohdat: `show` -funktio Haskellissa muuntaa päivämäärän vakioformaatiksi (ISO 8601), jossa päivämäärä ja aika erotetaan toisistaan välilyönnillä.

## Katso myös:

- [Haskell Data.Time dokumentaatio](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [ISO 8601-standardi Wikipediassa](https://fi.wikipedia.org/wiki/ISO_8601)
- [Python datetime moduulin dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [JavaScript Date-objektin dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)