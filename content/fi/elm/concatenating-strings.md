---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## Mikä & Miksi?
Yksinkertaisesti sanottuna merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon liittämistä yhteen, jolloin muodostuu uusi, pidempi merkkijono. Se on olennainen toiminto kaikille ohjelmoijille, jotta he voivat muodostaa dynaamisia lauseita tai viestejä.

## Näin se tehdään:

Elm-ohjelmointikielen merkkijonojen yhdistämisessä ```(++)``` operaattoria käytetään merkkijonojen yhdistämiseen. Tässä on koodiesimerkki:

```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text ( "Hei" ++ " maailma" )
```

Tämän ohjelman suorittaminen tuottaa seuraavan tulosteen:

```Elm
"Hei maailma"
```

## Syvällinen sukellus

Kuten useimmissa ohjelmointikielissä, myös Elm:ssä pitkä merkkijono voidaan muodostaa liittämällä yhteen useita pienempiä merkkijonoja. Tätä ominaisuutta on käytetty ohjelmoinnissa vuosikymmenien ajan, aina ensimmäisistä korkeamman tason ohjelmointikielistä alkaen.

Useimmissa ohjelmointikielissä on muitakin tapoja liittää merkkijonoja yhteen. Esimerkiksi JavaScriptissa on olemassa sekä perinteinen ```+``` operaattori että ```String.concat()``` funktio. Elm:ssä paras tapa merkkijonojen liittämiseen on ```(++)``` operaattori, joka on yksinkertainen ja tehokas.

Kun suoritat merkkijonojen yhdistämisen Elm:ssä, se tapahtuu tehokkaasti ja turvallisesti. Elm-kielen run-time ympäristö hoitaa kaiken tarvittavan muistinhallinnan, jolloin ohjelmoijan ei tarvitse huolehtia siitä.

## Katso myös

Lisätietoja merkkijonojen yhdistämisestä Elm-ohjelmointikielessä löydät seuraavista lähteistä:

- Elm-kielen virallinen dokumentaatio: [https://elm-lang.org/docs](https://elm-lang.org/docs)

---

Muista, että merkkijonojen yhdistämisen sujuva hallinta on tärkeä taito, joka auttaa sinua kirjoittamaan tehokkaampia ja joustavampia ohjelmia.