---
title:                "Merkkijonon pääkaupunkikirjaintaminen"
html_title:           "Elm: Merkkijonon pääkaupunkikirjaintaminen"
simple_title:         "Merkkijonon pääkaupunkikirjaintaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Stringien Isoilla Kirjaimilla Aloitaminen Elm Ohjelmoinnissa

## Mikä & Miksi?

Isoilla kirjaimilla aloittaminen muuntaa stringin ensimmäisen kirjaimen isoksi. Ohjelmoijat käyttävät tätä tehdäkseen teksteistä helpommin luettavia ja ymmärrettäviä.

## Kuinka Näin:

Seuraava koodiesimerkki näyttää, kuinka tämä toimii Elm-ohjelmointikielessä:

```Elm
import String exposing (left, toUpper, dropLeft)

capitalize : String -> String
capitalize string = 
    let
        firstLetter = String.left 1 string
        restOfTheString = String.dropLeft 1 string
    in 
        String.toUpper firstLetter ++ restOfTheString

main = 
    capitalize "hello"
```

Käynnistäessäsi tämän ohjelman, se tulostaa: 

```
"Hello"
```

## Syvällisempää Tietoa:

Historiallisesti useat ohjelmointikielet, kuten JavaScript ja Python, ovat sisällyttäneet tämän ominaisuuden standardikirjastoihinsa. Elm, ollen funktionaalisen ohjelmoinnin kieli, kuitenkin vaatii tekemään tämän manuaalisesti. 

On olemassa useita tapoja saavuttaa tämä Elm:ssä ja yllä näytetty on ainoastaan yksi niistä. Jotkut ohjelmoijat saattavat esimerkiksi jakaa stringin listaksi, käsitellä ensimmäisen alkioksen isolla kirjaimella ja yhdistää listan takaisin stringiksi. 

Jos käsittelet erittäin suurta määrää tietoa, voit huomata joitain suorituskyvyn vaihteluita eri toteutustapojen välillä. Stringin jakaminen ja yhdistäminen voi olla hitaampaa kuin ensimmäisen kirjaimen muuttaminen isolla kirjaimella suoraan stringissä.

## Katso Myös:

1. Elm String funktion dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/String
2. Elm yhteisön keskusteluja string manipulointi: https://discourse.elm-lang.org/t/string-manipulation-in-elm/5743
3. String manipuloinnin opas Elm:ssä: https://korban.net/posts/elm/2019-12-14-practical-guide-string-manipulation-elm/