---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Elm: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä tarkoittaa tietyn päivämäärän lisäämistä tai vähentämistä annetusta päivämäärästä. Tämä on tärkeä osa monia ohjelmointitehtäviä, kuten tapahtumien aikataulutusta ja aikamäärien laskemista. Ilman tällaista toiminnallisuutta ohjelmointitehtävät olisivat monimutkaisempia ja aikaa vievämpiä.

## Miten:

Elmissä päivämäärien laskeminen tulevaisuudessa tai menneisyydessä on helppoa ja vaivatonta käyttäen Date-pakettia. Seuraavassa esimerkissä lisätään yksi kuukausi annettuun päivämäärään ja tulostetaan uusi päivämäärä:

```
import Date exposing (add)
import Date exposing (fromCalendarDate)

fromCalendarDate 2021 7 19
    |> add 0 1 0
    |> toString
```
Tulostus: "2021-08-19"

## Syväsukellus:

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on ollut tärkeä osa ohjelmointia jo pitkään. Aikaisemmin tämä vaati monimutkaisia laskelmia ja käsittelyä, mutta nykyään monet ohjelmointikielet, kuten Elm, tarjoavat valmiita toimintoja, jotka helpottavat tätä tehtävää.

On myös olemassa muita vaihtoehtoja päivämäärien laskemiseen, kuten käyttäen UNIX-timelämpötiloja tai erilaisia kirjastoja ja paketteja. Elm-paketti tarjoaa kuitenkin helpon ja suoran tavan käsitellä päivämääriä ja tehdä laskelmia niiden kanssa.

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä vaatii tarkkaa tietoa kalenterijärjestelmistä ja aikavyöhykkeistä, joten on tärkeää olla huolellinen ja tarkka näitä asioita käsitellessään.

## Katso myös:

- [Elm Date-paketti](https://package.elm-lang.org/packages/elm/json/latest/)
- [Elm-ohjelmointikielen virallinen sivusto](https://elm-lang.org/)
- [Date and Time - This Changes Everything](https://youtu.be/UGVhytwmxTg) (video aiheesta)