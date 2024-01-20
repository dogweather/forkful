---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Luodaan satunnaislukuja Swift-ohjelmointikielessä

## Miksi & Miksi?
Satunnaislukujen tuottaminen on prosessi, jossa luodaan ennakoimaton luku valitulta lukuväliltä. Tätä tarvitaan usein ohjelmointipeleissä, tietoturva-sovelmuksissa ja tilastollisissa analyysiohjelmissa.

## Kuinka:
Seuraava esimerkki näyttää, kuinka luodaan satunnaisluku Swiftissä.

```Swift
import Swift

let randomInt = Int.random(in: 1..<10)
print(randomInt)
```

Kun ajat tämän koodiesimerkin, se tulostaa satunnaisen kokonaisluvun lukujen 1 ja 10 välillä.

## Syvempi sukellus:
Ennen Swift 4.2 -versiona, satunnaislukujen luominen vaati usein suoran pääsyn C-standardeihin, kuten `arc4random_uniform()`. Swift 4.2 ja uudemmat versiot ovat tehneet satunnaislukujen luomisesta huomattavasti yksinkertaisempaa and ja turvallisempaa `random(in:)` -funktion avulla.

Vaikka `random(in:)` onkin useimmissa tapauksissa kätevä, kannattaa silti ymmärtää sen rajoitukset. Sen toistettavuus ja ennustamattomuus voivat vaihdella käytetyn alustan ja Swift-version mukaan.

Joissakin tapauksissa saatat tarvita myös muita satunnaislukugeneraattoreita. Esimerkiksi `GameplayKit` tarjoaa useita erityyppisiä satunnaislukugeneraattoreita, jotka voivat olla hyödyllisiä tietyissä pelisovelluksissa.

## Katso Myös:
- Swiftin virallinen dokumentaatio satunnaislukujen luomisesta: [https://developer.apple.com/documentation/swift/int/2995648-random](https://developer.apple.com/documentation/swift/int/2995648-random)
- GameplayKitin dokumentaatio eri satunnaislukugeneraattorityypeistä: [https://developer.apple.com/documentation/gameplaykit/gkrandomsource](https://developer.apple.com/documentation/gameplaykit/gkrandomsource)