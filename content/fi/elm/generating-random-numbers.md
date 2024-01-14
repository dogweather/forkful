---
title:                "Elm: Satunnaislukujen generointi"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi käyttää satunnaislukugeneraattoria?

Satunnaislukujen luominen on tärkeä osa monia ohjelmointitehtäviä, kuten pelien ja simulaatioiden kehittämistä. Elmissä on sisäänrakennettu tapa generoida satunnaislukuja ja tässä blogikirjoituksessa käymme läpi miten se tapahtuu ja miten sitä voi hyödyntää omassa koodissa.

## Kuinka käyttää satunnaislukugeneraattoria Elmissä?

Elmissä satunnaislukuja voidaan generoida käyttäen `Random` -moduulia ja sen `generate` -funktiota. Alla on yksinkertainen esimerkki satunnaisen luvun generoimisesta ja sen tulostamisesta konsolille:

```Elm
import Random

randomNumber = 
    Random.generate
        Random.int 0 100

main = 
    randomNumber
        |> Random.map (\num -> "Satunnainen luku: " ++ String.fromInt num)
        |> Random.map Debug.log
```

Tämä koodinpätkä generoi luvun väliltä 0-100 ja tulostaa sen konsolille muodossa "Satunnainen luku: [luvun arvo]". Alla on mahdollinen tuloste tästä koodista:

```
Satunnainen luku: 32
```

Elm-maailman sisällä `Random` -moduulia käytetään yleensä yhdessä `Cmd.map` -funktion kanssa, joka mahdollistaa satunnaislukujen generoimisen asynkronisesti ja niiden käsittelyn `Msg` -tiedonkulussa.

## Syvemmälle satunnaislukujen generoimiseen Elmissä

`Random` -moduulissa on monia hyödyllisiä funktioita satunnaislukujen generoimiseen, kuten `float`, `bool` tai jopa `pair` kahden satunnaisen luvun generoimiseen. On myös mahdollista määrittää oma tapa generoida lukuja `Generator` -tyypin avulla.

Elm tarjoaa myös mahdollisuuden hallita satunnaislukugeneraattoreita ja niiden tilaa `Seed` -tyypin avulla. Tämä on erityisen hyödyllistä, kun tarvitaan toistettavia satunnaislukuja esimerkiksi testauksessa.

## Katso myös

- [Elm-satunnaislukugeneraattorin dokumentaatio](https://package.elm-lang.org/packages/elm/random/latest/)
- [Satunnaislukujen generointi JavaScriptillä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)