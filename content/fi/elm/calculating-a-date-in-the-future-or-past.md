---
title:    "Elm: Tiivistelmä: Päivän laskeminen tulevaisuudessa tai menneisyydessä tietokoneohjelmoinnissa"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Miksi lasken tulevia ja menneitä päivämääriä Elmilla?

Elm on funktionaalinen ohjelmointikieli, joka on suunniteltu helppokäyttöisyyden ja luotettavuuden periaatteilla. Laskeminen tulevia ja menneitä päivämääriä voi olla hyödyllistä esimerkiksi tapahtumien aikatauluttamisessa tai tietyn ajanjakson laskemisessa.

# Näin lasket päivämäärän tulevaisuudessa tai menneisyydessä

Laskeminen päivämääriä Elmilla on yksinkertaista käyttäen Date-pakettia. Alla on esimerkki koodista, jossa lasketaan päivä tulevaisuudessa ja tulostetaan se konsoliin:

```Elm
import Date exposing (..)

tulevaPaiva = add Days 10 (fromTime 1544191200000)

main = 
  2|
    Debug.log "Tuleva päivä" tulevaPaiva
```

Tämä koodi lisää 10 päivää nykyiseen päivään ja tulostaa tulevan päivän konsoliin. Voit muuttaa lisättävien päivien määrää ja saada eri päivämääriä tulevaisuudessa.

# Syväsukellus päivämäärän laskemiseen Elmilla

Date-paketti tarjoaa monia erilaisia funktioita päivämäärien laskemiseen, kuten vähentämisen, kuukausien tai vuosien lisäämisen ja muuntamisen Unix-aikaleimoiksi. Voit myös käyttää Date.Extra-moduulia, joka tarjoaa lisäominaisuuksia kuten päivämäärän muotoilun ja viikonpäivän laskemisen.

Voit myös käyttää tiettyjä päivämääriä vertaillessa funktiota Date.compare, joka palauttaa arvon `LT` (pienempi kuin), `GT` (suurempi kuin) tai `EQ` (yhtäsuuri). Tämä voi olla hyödyllistä esimerkiksi tapahtumien järjestämisessä päivämäärän mukaan.

# Katso myös

- [Date-paketin dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Date.Extra-moduulin dokumentaatio](https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest/)
- [Elm-kurssi - Ajastimen käyttö Elmilla](https://elmprogramming.com/elm-timer.html)