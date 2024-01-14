---
title:    "Haskell: Mallia vastaavien merkkien poistaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointitehtävissä joudutaan käsittelemään tekstejä ja poistamaan niistä tiettyjä merkkejä. Tässä blogikirjoituksessa käydään läpi, miksi ja miten voit poistaa merkkejä vastaten tiettyyn kaavaan Haskell-ohjelmointikielessä.

## Miten

Haskelissa on monia tapoja poistaa merkkejä tekstistä, mutta käsittelemme tässä kirjoituksessa yhtä yleistä tapaa, joka käyttää ```filter``` ja ```not``` funktioita. Oletetaan, että haluamme poistaa kaikki numerot merkkijonosta "H4sk3ll", jolloin haluttu tulos olisi "Hskll". Käytännössä tämä voidaan toteuttaa seuraavalla koodilla:

```Haskell
removeNumbers :: String -> String
removeNumbers str = filter (\x -> not (x `elem` ['0'..'9'])) str
main = putStrLn (removeNumbers "H4sk3ll")
```

Tässä esimerkissä funktio ```removeNumbers``` ottaa parametrinaan merkkijonon ja käyttää ```filter``` funktiota poistaakseen kaikki numerot merkkijonosta. Lopuksi ```main``` funktio tulostaa funktion palauttaman uuden merkkijonon. Huomaa, että käytimme anonyymia funktiota ```\x -> not (x `elem` ['0'..'9'])```, jolla tarkoitetaan funktiota, joka ottaa parametrinaan merkin ja palauttaa ```True``` jos se ei kuulu numeroihin ja ```False``` jos kuuluu.

## Syvemmälle

Tämä esimerkki on vain yksi tapa poistaa merkkejä Haskellissa. Voit myös käyttää muita funktioita, kuten ```map```, tai erilaisia ehtolausekkeita ```if```, ```case``` tai ```guard```, jotta voit räätälöidä poistettavia merkkejä tai kaavaa vielä paremmin oman tarpeesi mukaan.

## Katso myös
- [Haskellin dokumentaatio filter-funktiosta](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:filter)
- [Haskellin dokumentaatio not-funktiosta](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:not)
- [Haskellin dokumentaatio anonyymeistä funktioista](https://www.haskell.org/tutorial/functions.html#anonymous-functions)