---
title:    "Elm: Virheenjäljitys tulostus"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat joutuvat joskus käsittelemään ohjelmoinnin vikasietoa koodia. Tulostuslausekkeet voivat auttaa ratkaisemaan näitä ongelmia ja lisätä ohjelman luettavuutta.

## Miten

Elm-kielessä käytetään "Debug.log" -funktiota ​​tulostamaan debug-tietoja. Tämä funktio ottaa argumentikseen merkkijonon ja arvon, joka tulostetaan näytölle.

```Elm
Debug.log "viesti" 5
```

Tämä tulostaa "viesti : 5" konsolille tai selaimen kehitystyökaluihin.

## Syvällisemmin

"Debug.log" -funktio auttaa ohjelmoijaa näkemään, mitä koodi tekee tiettynä hetkenä suorituksen aikana. Näin ollen se voi auttaa tunnistamaan ongelmia ja virheitä ohjelmassa.

On myös mahdollista tulostaa monimutkaisempia arvoja, kuten lista tai tietue, "Debug.log" -funktion avulla.

```Elm
Debug.log "lista" [1, 2, 3]
```

Tämä tulostaa "lista : [1, 2, 3]" konsolille. Tämä voi auttaa ohjelmoijaa ymmärtämään paremmin ohjelman datan rakennetta.

## Katso myös

- [Elm-kurssi](https://guide.elm-lang.org/)
- [Elm-yhteisö](https://www.elm-community.org/)
- [Debug.log dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)