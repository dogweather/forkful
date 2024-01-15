---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Elm: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä saattaa olla tarpeen muuttaa päivämäärä merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi tietyn formaatin vaatimiseksi tai päivämäärän tallentamiseksi tietokantaan. Elm-kielellä tämän tehtävän suorittaminen on helppoa ja vaivatonta.

## Kuinka

Muuttaaksesi päivämäärän merkkijonoksi Elm-kielellä, sinun tarvitsee vain käyttää Date.toIsoString-funktiota. Se muuntaa annetun päivämäärän ISO 8601 -standardin mukaiseen merkkijonoon.

```Elm
import Date exposing (toIsoString)

date = Date.fromParts 2020 10 31

toIsoString date -- "2020-10-31"
```

Jos haluat muuttaa päivämäärän toiseen formaattiin, voit käyttää Date.format-funktiota ja antaa haluamasi formaattimerkkijonon parametrina.

```Elm
import Date exposing (format)

date = Date.fromParts 2020 10 31

format "DD.MM.YYYY" date -- "31.10.2020"
```

## Syvemmälle

Elm tarjoaa myös erilaisia Date-moduulin funktioita päivämäärän käsittelyyn. Voit tarkistaa dokumentaatiosta kaikki saatavilla olevat funktiot ja niiden käyttötarkoitukset.

Merkittävänä huomiona, Elm-kielessä päivämäärän muuttaminen merkkijonoksi on turvallista, sillä se käsittelee päivämäärän olion sijasta luotettavaa ISO 8601 -standardin mukaista merkkijonoa. Tämä varmistaa, että päivämäärän muunnos ja tulkinta tapahtuvat oikein eri ympäristöissä.

## Katso myös

- [Elm Date-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [ISO 8601 -standardin määrittely](https://www.iso.org/iso-8601-date-and-time-format.html)