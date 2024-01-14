---
title:                "Elm: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Miksi laskea tulevia tai menneitä päivämääriä?

On monia syitä, miksi haluat ehkä laskea päivämääriä tulevaisuudessa tai menneisyydessä. Ehkä tarvitset tätä ominaisuutta sovelluksessasi, jotta voit muistuttaa käyttäjiä tärkeistä tapahtumista tai laskuttaa tulevaa laskua. Tai ehkä haluat vain antaa käyttäjille mahdollisuuden nähdä, kuinka paljon päiviä on jäljellä heidän syntymäpäiväänsä. Joka tapauksessa, tässä on muutama yksinkertainen tapa laskea päivämääriä tulevaisuudessa tai menneisyydessä käyttäen Elm-ohjelmointikieltä.

## Miten lasketaan päivämääriä tulevaisuudessa tai menneisyydessä?

### Päivien lisääminen

Jos haluat lisätä päiviä tiettyyn päivämäärään, voit käyttää `addDays` -funktiota. Esimerkiksi:

```Elm
import Date exposing (addDays, fromParts)

-- Laskee viisi päivää tulevaisuudessa
addDays 5 (fromParts 2020 February 14)
-- Palauttaa Date-armeen 2020 helmikuu 19
```

### Kuukausien lisääminen

Jos haluat lisätä kuukausia tiettyyn päivämäärään, voit käyttää `addMonths` -funktiota. Esimerkiksi:

```Elm
import Date exposing (addMonths, fromParts)

-- Laskee neljä kuukautta tulevaisuudessa
addMonths 4 (fromParts 2020 February 14)
-- Palauttaa Date-armeen 2020 kesäkuu 14
```

### Vuosien lisääminen

Jos haluat lisätä vuosia tiettyyn päivämäärään, voit käyttää `addYears` -funktiota. Esimerkiksi:

```Elm
import Date exposing (addYears, fromParts)

-- Laskee kaksi vuotta tulevaisuudessa
addYears 2 (fromParts 2020 February 14)
-- Palauttaa Date-armeen 2022 helmikuu 14
```

### Päivien vähentäminen

Voit myös vähentää päiviä, kuukausia ja vuosia tietystä päivämäärästä käyttämällä vastaavia funktioita `subDays`, `subMonths` ja `subYears`. Esimerkiksi:

```Elm
import Date exposing (subMonths, fromParts)

-- Vähentää yhden kuukauden menneisyydessä
subMonths 1 (fromParts 2020 February 14)
-- Palauttaa Date-armeen 2020 tammikuu 14
```

### Huomioita

On hyvä tiedostaa, että nämä funktiot käyttävät uutta päivämäärätyyppiä, joka tuli käyttöön Elm-versiossa 0.19. Jos käytät vanhempaa versiota Elmistä, sinun pitäisi käyttää `Date.add` ja `Date.subtract` -funktioita sen sijaan. Voit tarkistaa käyttämäsi Elm-version `elm.json` -tiedostosta projektisi juuresta.

## Syventävä tutkimus

Jos haluat syvemmän katsauksen päivämäärien laskemiseen tulevaisuudessa tai menneisyydessä, hyvä lähtökohta on tutustua `Date` -moduuliin ja sen tarjoamiin erilaisiin funktioihin ja tietotyyppeihin. Voit myös suorittaa lisätutkimusta päivämäärien käsittelystä yleisesti, jotta ymmärrät paremmin niiden toimintaperiaatteita.

## Katso myös

- [Elm-opp