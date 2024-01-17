---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Elm: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärän muuntaminen merkkijonoksi on yksinkertainen tapa näyttää päivämäärät käyttäjälle ymmärrettävässä muodossa. Ohjelmoijat käyttävät tätä esimerkiksi verkkosivuilla, sovelluksissa ja tietokoneohjelmissa.

## Kuinka tehdä?

Elm-kielellä päivämäärän muuntaminen merkkijonoksi on helppoa. Seuraavassa esimerkissä käytämme avuksi Date-moduulia ja sen funktiota `toIsoString`:

```Elm
import Date

Date.toIsoString (Date.fromCalendarDate 2021 8 23)
```

Tämä koodi tuottaa merkkijonon `2021-08-23`, joka vastaa päivämäärää 23.8.2021. Voit myös muuttaa muodostettavan merkkijonon formaattia lisäämällä halutut välimerkit `toIsoString`-funktion parametreihin.

## Syvemmälle

Päivämäärän muuntaminen merkkijonoksi on tärkeä osa tietokoneohjelmointia, joten siihen on olemassa useita eri tapoja eri ohjelmointikielillä. Elm-kielessä tämä on kuitenkin mahdollista vain Date-moduulilla, sillä kieli on suunniteltu välttämään ylimääräisiä riippuvuuksia.

## Katso myös

- [Date-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm-kielen kotisivut](https://elm-lang.org/)