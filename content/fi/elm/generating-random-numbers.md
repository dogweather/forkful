---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:12.571103-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Satunnaisluvut ovat arvaamattomia numeroita. Ohjelmoijat käyttävät niitä simuloidakseen arpajaisia, luomaan pelimekaniikkaa, tai testaamaan ohjelmistoja satunnaisilla tiedoilla.

## How to: (Kuinka tehdään:)
Elm käyttää pakettia `Random` satunnaislukujen generointiin. Tässä on esimerkki siitä, kuinka luoda satunnaisluku väliltä 1-100.

```Elm
import Random

-- Satunnaisarvon tyyppi
type alias Seed =
    Random.Seed

-- Alkuarvo (seed), tässä käytetään sen hetkistä aikaa
initialSeed : Seed
initialSeed =
    Random.initialSeed (Time.millisToPosix <| Date.now)

-- Generoi satunnainen Int väliltä 1-100
randomInt : Seed -> ( Int, Seed )
randomInt seed =
    Random.step (Random.int 1 100) seed
```
Tämän koodin suorittaminen antaa satunnaisen numeron ja uuden seedin tulostuksena.

## Deep Dive (Syväsukellus):
Historiallisesti satunnaislukugeneraattoreita on kehitetty matemaattisten algoritmien kautta. Elm käyttää funktionaalista lähestymistapaa, jossa uusi satunnaisluku riippuu aina "seed" eli alkuarvosta. Se takaa saman sarjan numeroita samalla seedillä. Tämä on käytännöllistä testeissä.

Vaihtoehtoisesti, voit käyttää `Random.Float` funktiota desimaalilukujen generointiin tai `Random.list` satunnaisten listojen luomiseen. Elm erottuu sillä, että se hoitaa satunnaisuuden puhtaasti funktionaalisesti ilman tilan muutoksia.

## See Also (Katso Myös):
- Elm Random moduulin dokumentaatio: https://package.elm-lang.org/packages/elm/random/latest/
- Seeds and randomness in functional programming: https://elm-lang.org/0.19.0/random
- Matemaattiset algoritmit satunnaislukujen takana (ei Elm-spesifinen): https://en.wikipedia.org/wiki/List_of_random_number_generators
