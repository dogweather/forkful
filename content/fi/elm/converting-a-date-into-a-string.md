---
title:                "Elm: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Elm-ohjelmointi kiehtoo monia ohjelmoijia sen yksinkertaisuuden ja vakaan rakenteen takia. Mutta yksinkertaisuus ei tarkoita, että sen ominaisuudet ovat rajoitettuja. Tässä artikkelissa tutustumme siihen, kuinka muunnetaan päivämäärä merkkijonoksi Elm-ohjelmassa ja miksi tämä on hyödyllistä.

## Kuinka tehdä
Koodiesimerkkien avulla näytämme, kuinka päivämäärä voidaan muuntaa merkkijonoksi käyttämällä Elm-kirjastojen tarjoamia valmiita toimintoja. Käyttämällä funktioita kuten `format`, `Date.toMillis` ja `Time.Posix`, voimme helposti muuttaa päivämäärän halutun muotoiseksi ja tulostaa sen merkkijonona.

```Elm
import Date exposing (Date)
import Date.Format as Format
import Time exposing (Posix)

date : Date
date =
  Date.fromCalendarDate 2020 1 15

Format.custom "dd.MM.yyyy" (Date.toMillis date)
    -- Output: "15.01.2020"

Format.rfc2822 (Posix.millisToPosix (Date.toMillis date))
    -- Output: "Wed, 15 Jan 2020 00:00:00 +0000"
```

## Syvällinen sukellus
Päivämäärän muuntaminen merkkijonoksi on hyödyllistä, kun ohjelmoijan täytyy näyttää päivämäärä käyttäjälle selkeässä ja ymmärrettävässä muodossa. Mutta tämä toiminto voi myös olla hyödyllinen datan käsittelyssä ja vertailussa. Esimerkiksi voimme käyttää `Date.toMillis`-funktiota muuttaaksemme päivämäärän millisekunteina, jolloin voimme helposti vertailla kahta päivämäärää ja laskea niiden välisen eron.

## Katso myös
- [Date-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Time-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Time)