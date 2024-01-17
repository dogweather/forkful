---
title:                "Sattumaisten lukujen generointi"
html_title:           "Haskell: Sattumaisten lukujen generointi"
simple_title:         "Sattumaisten lukujen generointi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaisten numeroiden generointi on ohjelmistokehittäjien tapa tuottaa satunnaisia lukuja ohjelmilleen. Tämä voi olla hyödyllistä muun muassa pelien ja arvontojen yhteydessä.

## Kuinka?
Haskellin "System.Random" -kirjaston avulla voimme generoida satunnaisia kokonaislukuja sekä liukulukuja. Katso alla olevia esimerkkejä ja niiden tulosteita.

```Haskell
import System.Random

-- Satunnainen kokonaisluku väliltä 0-10
randomNumber :: IO Int
randomNumber = randomRIO (0, 10)

-- Satunnainen liukuluku väliltä 0-1
randomFloat :: IO Float
randomFloat = randomRIO (0, 1)

-- Tuloste: 7
-- Tuloste: 0.5234987
```

## Syvää sukellusta
Satunnaislukugenerointi on tärkeä osa monien ohjelmien toimintaa ja on ollut osa ohjelmointi kieliä jo vuosikymmenien ajan. Vaikka "System.Random" -kirjasto on yksi tapa generoida satunnaisia lukuja, muita vaihtoehtoja ovat esimerkiksi "random" ja "random-fu" -kirjastot. Satunnaislukujen generointi perustuu usein pseudo-satunnaislukujen algoritmeihin, jotka pyrkivät tuottamaan lukuja, jotka vaikuttavat satunnaisilta mutta toistavat kuitenkin tietyn kaavan mukaan.

## Katso myös
- [System.Random - Hackage](https://hackage.haskell.org/package/random)
- [Random number generation - Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)
- [Random numbers in Haskell: an empirical study - Colin Runciman](https://www.cs.york.ac.uk/fp/randoms.pdf)