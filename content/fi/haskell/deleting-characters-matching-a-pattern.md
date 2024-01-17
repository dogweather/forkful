---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Haskell: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & miksi?

Haskellissa merkkijonojen käsittely on helppoa ja kätevää. Yksi hyödyllinen toiminto on poistaa merkkejä, jotka täsmäävät tiettyyn malliin. Tämä voi olla hyödyllistä esimerkiksi tekstin analysoinnissa tai tietynlaisen datan etsimisessä. Monet ohjelmoijat käyttävät tätä toimintoa puhdistaakseen dataa tai muokatakseen merkkijonoja tarvittavaan muotoon.

## Kuinka tehdä:

Tässä on esimerkki siitä, kuinka voimme poistaa kaikki numerot sisältävät merkit merkkijonosta nimeltä "hae tuotteet". Tämä voidaan tehdä käyttämällä funktiota ```map```, joka suorittaa annetun funktion jokaiselle listan alkiolle.

```Haskell
removeNumbers string = map (\x -> if not (x `elem` ['0'..'9']) then x else ' ') string

removeNumbers "hae tuotteet" -- Output: "hae tuotteet"
removeNumbers "hae 123 tuotteet" -- Output: "hae tuotteet"
```

## Syvemmälle:

Historiallisesti Haskell on tunnettu puhtaasta funktionaalisesta ohjelmointityylistään, jolla on vahva tuki erilaisille tietorakenteille ja rekursiolle. Jos haluat oppia lisää funktionaalisesta ohjelmoinnista ja Haskellin ominaisuuksista, suosittelemme tutustumaan "Learn You a Haskell" -verkkosivustoon. Lisäksi voit käyttää myös muita funktioita, kuten ```filter``` ja ```foldl```, poistaaksesi merkkejä haluamallasi tavalla.

## Katso myös:

- [Learn You a Haskell](http://learnyouahaskell.com/): Opettele Haskellin perusteet interaktiivisen oppaan avulla.
- [Haskell-ohjelmointikieli](https://fi.wikipedia.org/wiki/Haskell): Tietoa Haskellista ja sen historiasta (on huomattava, että Wikipedia-artikkelit eivät ole parhaita lähteitä oppimiseen).