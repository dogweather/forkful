---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Haskell: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Laskeminen päivämäärä tulevaisuudessa tai menneisyydessä tarkoittaa uuden päivämäärän muodostamista nykyisestä päivämäärästä lisäämällä tai vähentämällä päiviä. Ohjelmoijat tekevät tämän, kun heidän pitää suorittaa päivämäärän perusteella tapahtuvia toimintoja, kuten aikavälien laskemista tai päivämäärien vertailua.

## Miten:

Haskell-ohjelmointikielessä voimme käyttää 'Data.Time' -kirjastoa. Kas, tässä on pieni koodiesimerkki:

```Haskell
import Data.Time

main = do
    let tänään = fromGregorian 2021 9 15
    let viikonPäästä = addDays 7 tänään
    putStrLn $ "Viikon päästä se on " ++ show viikonPäästä
```
Tämän suorittaminen tulostaa: "Viikon päästä se on 2021-09-22".

## Syvempi sukellus

### Historiallinen konteksti
Ajan laskenta on ollut keskeisessä asemassa ohjelmoinnissa sen alkupäivistä saakka. Suurin haaste ajan käsittelyssä on erilaisten aikavyöhykkeiden, päivämäärien ja kellonaikojen käsittely eri maissa ja alueilla. Tässä onkin Haskellin 'Data.Time'-kirjaston vahvuus, sillä se tarjoaa monipuoliset työkalut näiden haasteiden kohtaamiseksi.

### Vaihtoehdot
Voit myös käyttää 'Data.Time.Clock'-kirjastoa, jos haluat suorittaa laskutoimituksia kellonaikojen kanssa. Se toimii hyvin yhdistettynä 'Data.Time'-kirjastoon.

### Toteutuksen yksityiskohdat
'addDays'-funktio on olennainen osa päivämäärien laskemista Haskellissa. Se ottaa päivien määrän ja 'Day'-olion parametreikseen, ja palauttaa uuden 'Day'-olion joka on selkeä ja helppo tapa suorittaa päivien lisäystä tai vähennystä.

## Katso myös:

1. [Haskell Data.Time kirjaston dokumentaatio](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html): Täydellistä tietoa Haskellin ajan ja päivämäärän käsittelyn kirjastosta. 
2. [Kellonaikojen käsittely Haskellissa](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html): Opas kellonaikojen laskennasta Haskellilla. 
3. [Haskellista wikipediassa](https://fi.wikipedia.org/wiki/Haskell): Lisätietoa Haskellin ohjelmointikielestä.