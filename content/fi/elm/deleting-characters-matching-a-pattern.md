---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Elm: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Olet ehkä törmännyt tilanteeseen, jossa sinun täytyy poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla tarpeellista, jos haluat esimerkiksi poistaa kaikki välilyönnit tai poistaa erikoismerkit käyttäjän syötteestä. Käytännössä tällä toiminnolla on monia eri käyttötarkoituksia, ja se voi tehdä ohjelmoinnista paljon helpompaa.

## Kuinka tehdä

Voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa Elm-kielellä helposti käyttämällä `String.filter` -funktiota. Tämä funktio ottaa kaksi argumenttia: kaavan ja merkkijonon, josta haluat poistaa merkkejä. Tässä on yksinkertainen esimerkki, jossa poistamme kaikki välilyönnit merkkijonosta:

```elm
import String exposing (..)

removeSpaces : String -> String
removeSpaces str =
  str
    |> filter (\char -> char /= " ")
```

Tämä funktio ottaa merkkijonon ja kutsuu `filter` -funktiota, jossa jokainen merkki tarkistetaan kaavaa vastaan. Jos merkki ei vastaa kaavaa (eli ei ole välilyönti), se pysyy merkkijonossa. Muut merkit, jotka vastaavat kaavaa, jätetään pois. Tässä on esimerkki, jossa käytämme tätä funktiota ja tulostamme lopputuloksen:

```elm
main : Html msg
main =
  text (removeSpaces "Tämä on  esimerkkiteksti!")
```

Tulostus tulee olemaan:

```
"Tämäonesimerkkiteksti!"
```

Voit käyttää tätä samaa periaatetta poistaaksesi erilaisia merkkejä ja luoda omia toimintoja, jotka vastaavat tiettyjä kaavoja.

## Syvällinen sukellus

Filtteröinti on vain yksi tapa poistaa merkkejä Elm-kielellä. On myös muita vaihtoehtoja, kuten käyttää `String.replace` -funktiota, joka vaihtaa kaavan vastaavat merkit toisilla merkeillä. Voit myös käyttää `String.left` ja `String.right` -funktioita, jotka palauttavat merkkijonon vasemmanpuoleiset ja oikeanpuoleiset merkit tietyltä indeksiltä. Näiden toimintojen yhdistelmällä voit suorittaa monimutkaisempia merkkien poistoja.

Voit myös hyödyntää reguläärejä ilmauksia (regex), jotka ovat erittäin voimakkaita työkaluja merkkien poistamiseen. Elm tekee yhteistyötä JS-kirjaston kanssa käsitelläkseen regexejä, joten voit käyttää samoja sääntöjä kuin JavaScriptillä.

## Katso myös

- [Elm-dokumentaatio merkkijonofunktioista](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Blogipostaus, jossa käsitellään merkkijonojen käsittelyä Elm-kielellä](https://medium.com/swlh/working-with-strings-in-elm-e13976653c9f)
- [Regex-oppaat ja -esimerkit](https://regexone.com/)