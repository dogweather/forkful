---
title:                "Elm: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointikielissä päivämäärän muuttaminen merkkijonoksi voi olla monimutkaista ja haastavaa. Elm-kielessä tämä prosessi on kuitenkin yksinkertaista ja helppoa, mikä tekee siitä hyvän vaihtoehdon päivämäärämuotojen käsittelyyn.

## Miten

Elm-kielessä päivämäärän muuttaminen merkkijonoksi tapahtuu `Date.toIsoString`-funktiolla. Tämä funktio ottaa parametrinaan päivämäärämuuttujan ja palauttaa sen merkkijonona ISO 8601 -standardin mukaisessa muodossa.

```Elm
import Date exposing (..)

dateToString : Date -> String
dateToString date =
  Date.toIsoString date

main =
  dateToString (Date.fromYMD 2021 03 08)

-- Output: "2021-03-08"
```

## Syvempi sukellus

Elm-kielessä päivämäärät ovat kokonaislukuja, joiden arvo on millisekunteina kulunut aika 1. tammikuuta 1970 klo 00:00:00 UTC:sta lähtien. `Date.fromYMD`-funktio muuntaa annetut vuosi, kuukausi ja päivä kokonaisluvuksi, jota `Date.toIsoString`-funktio käyttää muodostaakseen oikeanlaisen merkkijonon.

## Katso myös

- [Elm-kieen virallinen dokumentaatio](https://guide.elm-lang.org/dates_and_times.html)
- [ISO 8601 -standardi](https://en.wikipedia.org/wiki/ISO_8601)