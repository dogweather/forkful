---
title:    "Swift: Satunnaisten numeroiden luominen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi ohjelmoijat haluavat luoda satunnaisia numeroita. Satunnaislukuja voidaan käyttää esimerkiksi testaamaan koodia, tekoälyn luomiseen tai pelien kehittämiseen.

## Kuinka tehdä

Satunnaislukujen generoiminen Swiftissä on helppoa. Voit käyttää sisäänrakennettua `arc4random()`-funktiota, joka palauttaa satunnaisen luvun alueelta 0 - (2^32 - 1). Voit myös rajoittaa lukujen alueen annettuihin lukuihin käyttämällä `arc4random_uniform(_:)`-funktiota.

```Swift
// Satunnainen luku väliltä 0 - (2^32 - 1)
let randomNumber = arc4random()

// Satunnainen luku välillä 1 - 10
let randomNumber = arc4random_uniform(10) + 1
```

## Syvemmälle

Satunnaislukujen generoiminen perustuu pseudosatunnaislukujen luomiseen. Tämä tarkoittaa, että vaikka luvut vaikuttavat satunnaisilta, ne ovat todellisuudessa ennalta määritettyä järjestystä. Swiftin sisäänrakennettu `arc4random()`-funktio käyttää kuitenkin riittävän monimutkaista algoritmia, joka tekee luvuista käytännössä satunnaisia.

## Katso myös

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID272)
- [Random-numerogeneraattori Swiftissä](https://www.hackingwithswift.com/example-code/language/how-to-generate-random-numbers-in-swift)
- [Satunnaislukujen käyttö pelinkehityksessä Swiftissä](http://www.swift-tutorial.com/how-to-use-random-numbers-in-game-development-with-swift/)