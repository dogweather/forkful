---
title:    "Swift: Satunnaisten lukujen luominen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi
Monet ohjelmat vaativat satunnaisten numeroiden generoimista erilaisiin tarkoituksiin, kuten pelien kehittämiseen tai simulointeihin. On tärkeää ymmärtää, miten tämä voidaan tehdä tehokkaasti ja luotettavasti Swift-ohjelmointikielellä.

## Miten
Satunnaislukujen generoiminen Swiftissä on helppoa käyttämällä built-in `arc4random()` -funktiota. Tämä antaa meille satunnaisen numeron väliltä 0 ja UINT32_MAX väliltä. Voimme myös määrittää ala- ja ylärajat käyttämällä `arc4random_uniform()` -funktiota. Esimerkiksi:

```Swift
let randomInt = arc4random()
print(randomInt) // tulostaa satunnaisen numeron väliltä 0 ja UINT32_MAX välillä

let randomIntBetween10And20 = arc4random_uniform(11) + 10
print(randomIntBetween10And20) // tulostaa satunnaisen numeron väliltä 10 ja 20 välillä
```

## Deep Dive
Swift mahdollistaa myös tarkemman satunnaislukugeneroinnin käyttämällä `arc4random_buf()` -funktiota, joka täyttää annetun muistialueen satunnaisilla biteillä. Tämä on hyödyllistä, jos halutaan generoida esimerkiksi salausavaimia tai muita turvallisuuteen liittyviä tietoja. Lisäksi `arc4random_uniform()` ei ole täysin satunnainen, vaan se on tarkoitettu käytettäväksi tilanteissa, joissa on tarpeen tasaisesti jakaa satunnaisia numeroita, kuten pelikorteissa tai noppafunktionaalisuuksissa.

## Katso myös
- [Swiftin virallinen dokumentaatio satunnaislukujen generoinnista](https://developer.apple.com/documentation/swift/1524096-arc4random)
- [Ray Wenderlichin osaava ohje satunnaislukujen generoinnista Swiftissä](https://www.raywenderlich.com/100368/random-numbers-in-swift)