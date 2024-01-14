---
title:    "Elm: Tekstin etsiminen ja korvaaminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Monilla ohjelmointikielillä on mahdollista suorittaa tekstien etsimistä ja korvaamista. Tässä artikkelissa käsitellään sitä, miten sama saavutetaan Elm-ohjelmointikielessä.

## Miten tehdä

"```Elm
-- Etsi ja korvaa funktion avulla
printText : String -> String
printText input =
  String.replace "vanha sana" "uusi sana" input 
```

"```Elm
-- Suorita etsintä ja korvaus tehtävä aktivoinnin yhteydessä
main =
  printText "Tämä on vanha sana"
  
-- Output: "Tämä on uusi sana" 
```

Voit myös käyttää `String.replace`-toimintoa suoraan tehtävään, kuten seuraavassa esimerkissä:

"```Elm
-- Vaihda kaikki isoille kirjaimille pienille kirjaimille
text : String
text = "TEKSTI tähän"

String.replace "TEKSTI" "teksti" text 

-- Output: "teksti tähän"
```

## Syvempää tietoa

Elm:ssä on valmis `String.replace`-funktio, joka löytyy `elm/core`-kirjastosta. Voit myös luoda oman funktion, joka suorittaa halutun etsinnän ja korvauksen, kuten ensimmäisessä esimerkissä.

On myös hyvä huomata, että tekstien etsiminen ja korvaaminen on mahdollista myös monimutkaisemman pattern matchaamisen avulla.

## Katso myös

- [Elm virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Elm Slack-yhteisö](https://elmlang.slack.com/)
- [Elm konferenssit ja tapahtumat](https://elm-conf.com/)