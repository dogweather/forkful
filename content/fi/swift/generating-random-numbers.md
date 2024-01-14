---
title:                "Swift: Sattumanvaraisten numeroiden luominen"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi haluaisit luoda satunnaisia numeroita Swift-ohjelmointikielellä? Satunnaisilla numeroilla on monia käyttötarkoituksia, kuten pelien luomiseen, testidataa generoimiseen ja satunnaisien laskelmien suorittamiseen.

## Kuinka tehdä se?

Voit luoda satunnaisen numeron Swiftillä käyttämällä `arc4random()` -funktiota. Tämä funktio palauttaa kokonaisluvun väliltä 0 ja 2^32 - 1. Seuraavassa esimerkissä luodaan ja tulostetaan 10 satunnaista numeroa väliltä 1 ja 100:

```Swift
for _ in 1...10 {
  let randomNum = arc4random_uniform(100) + 1
  print(randomNum)
}
```

Tässä koodissa käytetään myös `arc4random_uniform()` -funktiota, joka palauttaa satunnaisen numeron väliltä 0 ja haluttu maksimiarvo välttäen tasajakoa. Lisäksi voidaan käyttää muita esim. `arc4random()` -funktion muunnelmia, kuten `arc4random_uniform(max: UInt32)` ja `arc4random_uniform(min: UInt32, max: UInt32)`.

## Syvemmälle aiheeseen

Satunnaiset numerot Swiftissä luodaan käyttämällä Mersenne Twister -algoritmia, joka luo kyseisen satunnaisluokan. Tämä algoritmi on suunniteltu luomaan erittäin laadukkaita satunnaislukuja ja se on skaalautuva sekä suurelle että pienelle kokoonpanolle.

Voit myös antaa `srand()` -funktiolle alkuperäisen siemenarvon luodaksesi erilaisen satunnaisjonojen sarjan tai käyttää `srandom()` -funktiota muuttaaksesi siemenarvoa kesken ohjelman suorituksen.

## Katso myös

- [Swiftin eri random-funktiot](https://developer.apple.com/documentation/swift/global_functions/1539600-arc4random-uniform)
- [Mersenne Twister -algoritmi](https://en.wikipedia.org/wiki/Mersenne_Twister)