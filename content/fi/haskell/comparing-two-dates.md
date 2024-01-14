---
title:                "Haskell: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahden päivämäärän eroa?

Päivämäärän vertaileminen on tärkeä osa ohjelmointia, esiintyy usein esimerkiksi sovelluksissa, jotka käsittelevät varauksia tai aikatauluja. Se auttaa myös käyttäjiä löytämään tiettyjä tietoja tietystä ajanjaksosta tai selvittämään, onko tietty tapahtuma jo tapahtunut vai tapahtuuko se tulevaisuudessa.

## Kuinka vertailla kahden päivämäärän eroa?

Päivämäärän vertailu Haskellissa on helppoa, ja se onnistuu käyttämällä Standard Librariesin DateTime-moduulia. Ensimmäinen askel on tuoda DateTime-moduuli sisään.

```
import Data.Time
```

Seuraavaksi voimme määrittää kaksi päivämäärää, esimerkiksi nykyisen päivämäärän ja tulevan päivämäärän.

```
tämäpäivä <- utctDay <$> getCurrentTime
tulevapäivä <- return $ fromGregorian 2021 1 1
```

Ja lopuksi voimme käyttää `diffDays` -funktiota, joka palauttaa kahden päivämäärän välisen päivien erotuksen.

```
roju <- return $ diffDays tulevapäivä tämäpäivä
```

Saamme lopputulokseksi 257 päivää. Voimme myös käyttää muita funktioita, kuten `diffHours` tai `diffMonths` päivien sijasta, jos haluamme erilaisia vertailuja.

## Syvä sukellus päivämäärän vertailuun

Haskellin Standard Librariesin DateTime-moduulissa on monia muita hyödyllisiä funktioita ja työkaluja päivämäärän vertailuun. Esimerkiksi `addDiffTime` -funktio voi auttaa lisäämään tai vähentämään aikaa tietystä päivämäärästä. `getTimeZone` -funktio auttaa löytämään tietyn aikavyöhykkeen päivämäärälle ja `parseTimeM` voi auttaa muuntamaan päivämäärän eri muotoihin.

Päivämäärän vertailu voi myös olla haastavaa, sillä on tärkeää huomioida aikavyöhyke- ja kesäaikaongelmat sekä vuoden vaihteen aiheuttamat poikkeukset. Siksi on tärkeää käyttää oikeita työkaluja ja tarkistaa dokumentaatio tarvittaessa.

## Katso myös

- [DateTime-moduulin dokumentaatio](http://hackage.haskell.org/package/datetime)
- [Haskell-ohjelmoinnin perusteet](https://haskell.org/) 
- [Päivämäärän vertailun strategiat muilla ohjelmointikielillä](https://www.baeldung.com/java-date-compare)