---
title:                "Lauseiden yhdistäminen"
html_title:           "Elm: Lauseiden yhdistäminen"
simple_title:         "Lauseiden yhdistäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Stringien yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan esittää tekstiä dynaamisesti tai yhdistää useita muuttujia yhdeksi viestiksi.

## Kuinka:

Esimerkki stringien yhdistämisestä Elm-koodilla:

```
Elm
myString = "Tervetuloa "
myName = "Valtteri"
mergedString = myString ++ myName
```
Tulostettu tulos:
```
"Tervetuloa Valtteri"
```

## Syväsukellus:

Stringien yhdistämisen historia on alkujaan peräisin 60-luvulta, kun ensimmäiset ohjelmointikielet alkoivat käyttää merkkijonoja. Nykyäänkin useimmat kielet tarjoavat samanlaisen toiminnon, mutta joissakin kielissä se tapahtuu käyttämällä erilaisia operaattoreita, kuten `+` tai `&`. Elm-kielessä käytetään `++` merkkiä stringien yhdistämiseen.

Mahdollisia vaihtoehtoja concatenating stringsille ovat esimerkiksi käyttää `String.join` funktiota tai rakentaa uusi merkkijono käyttämällä `List.map` -funktiota.

Stringien yhdistäminen on tehokasta Elm-kielessä, sillä merkkijonojen muuttamista ei tarvitse tehdä sisäisesti. Koodin suorituksen aikana käytetään uutta merkkijonoa, jolla on yhdistetty arvo, sen sijaan että luotaisiin uutena merkkijonona useita versioita.

## Katso myös:

[Elm Guide - Strings](https://guide.elm-lang.org/types/strings.html)

[Elm String Library](https://package.elm-lang.org/packages/elm/core/latest/String)