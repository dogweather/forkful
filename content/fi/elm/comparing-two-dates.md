---
title:    "Elm: Kahden päivämäärän vertailu"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertailu on tärkeää monessa Elm-ohjelmointiprojektissa, sillä se auttaa meitä selvittämään, onko yksi päivämäärä ennen toista tai ovatko ne samat. Tämä voi olla hyödyllistä esimerkiksi tapahtumien järjestämisessä tai käyttäjien syötteiden validoimisessa.

## Näin vertailet päivämääriä Elm-ohjelmassa

Vertaileminen kahden päivämäärän välillä on helppoa Elm:ssä käyttämällä `Date`-moduulia. Voit käyttää `Date.compare`-funktiota, joka palauttaa `-1`, `0`, tai `1` riippuen siitä, mikä päivämäärä on aikaisempi. Voit myös käyttää muita funktioita, kuten `Date.isBefore`, `Date.isAfter`, tai `Date.equals` riippuen siitä, millaista vertailua haluat tehdä.

```Elm
import Date exposing (..)

january1 = Date.fromCalendarDate 2020 1 1
january2 = Date.fromCalendarDate 2020 1 2

Date.compare january1 january2 == -1

january1 `Date.isBefore` january2 == True
```

## Syvemmälle päivämäärien vertailuun

Vertaillessasi päivämääriä, on tärkeää ymmärtää, että ne on tallennettu tietynlaiseen muotoon Elm:ssä. Päivämäärät tallennetaan UTC-aikavyöhykkeeseen liittyen, mikä saattaa aiheuttaa eroja eri aikavyöhykkeiden välillä. Myös päivämäärille on olemassa erilaisia muotoiluvaihtoehtoja, kuten ISO-8601, joten varmista, että käytät oikeaa muotoa vertaillessasi niitä.

## Katso myös

- [Date moduuli Elm Documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [ISO-8601 muotoilu Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
- [Päivämäärän vertailu artikkeli Medium:ssa (eng.)](https://medium.com/@jessengatai/date-comparison-in-elm-how-does-it-work-bac9067fbae3)