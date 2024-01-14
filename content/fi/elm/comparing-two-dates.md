---
title:                "Elm: Kahden päivämäärän vertailu"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää Elm-ohjelmoinnissa?

On olemassa useita syitä, miksi vertailla kahta päivämäärää Elm-ohjelmoinnissa. Ensinnäkin, saattaa olla tarpeen tarkistaa, ovatko kaksi päivämäärää samat, esimerkiksi tarkistaaksesi, onko muutamassa päivämääräolemassa sama syntymäpäivä. Toiseksi, vertailemalla päivämääriä voi tarkistaa, onko jokin päivämäärä aikaisempi tai myöhäisempi kuin toinen. Tämä voi olla hyödyllistä järjestettäessä tietoa aikajärjestykseen tai tarkistettaessa, onko tietty tapahtuma tapahtunut ennen toista tapahtumaa.

## Kuinka vertailla kahta päivämäärää Elm-ohjelmoinnissa?

Vertaileminen kahden päivämäärän välillä Elm-ohjelmoinnissa on yksinkertaista. Ensinnäkin, käytä funktiota "Date.compare", joka palauttaa joko "Less", "Equal" tai "Greater" riippuen siitä, onko ensimmäinen päivämäärä ennen, sama tai myöhemmin kuin toinen. Voit esimerkiksi käyttää seuraavaa koodia:

```
elm make CompareDates.elm
```

Sitten anna seuraava koodi "CompareDates.elm" -tiedostoon:

```
import Date
main =
    case Date.compare (Date.fromParts 1995 12 17 ) (Date.fromParts 1995 12 17) of
        Date.Less   -> "First date is earlier than second date"
        Date.Equal  -> "Dates are the same"
        Date.Greater-> "Second date is earlier than first date"
```

Kun käytät "elm make" -komentoa, saat seuraavan tulosteen:

```
Dates are the same
```

## Syvemmälle päivämäärän vertailuun

Voit myös tarkastella muita tapoja vertailla päivämääriä Elm-ohjelmoinnissa. Esimerkiksi voit tarkistaa, onko jokin päivämäärä tietyn aikavälin sisällä toisesta päivämäärästä käyttämällä funktiota "Date.within", joka palauttaa "True" tai "False". Voit myös laskea päivien tai vuosien eron kahden päivämäärän välillä käyttämällä funktiota "Date.differenceInDays" tai "Date.differenceInYears". Tutustu Elm-dokumentaatioon lisätietoja varten.

## Katso myös

- [Elm virallinen dokumentaatio] (https://guide.elm-lang.org/)
- [Date-moduulin dokumentaatio] (https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Vertailufunktioiden ohjevideo] (https://www.youtube.com/watch?v=Z4amHX3mwfg)