---
title:                "Satunnaisten lukujen luominen"
html_title:           "Swift: Satunnaisten lukujen luominen"
simple_title:         "Satunnaisten lukujen luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

Mitä ja miksi?
Sattumanvaraisten numeroiden generoiminen on yksinkertaisesti prosessi, jossa ohjelma tuottaa satunnaisia numeroita. Tätä tarvitaan usein ohjelmoinnissa, esimerkiksi pelien luomisessa tai satunnaisten elementtien lisäämisessä sovelluksiin.

Miten se tehdään:
Swiftissä sattumanvaraisten numeroiden generoiminen on helppoa. Alla on esimerkki, kuinka generoida satunnaisia kokonaislukuja ja liukulukuja.

```
Swift 
// Generoi satunnainen kokonaisluku väliltä 1-100
var randomNumber = Int.random(in: 1...100)

// Generoi satunnainen liukuluku väliltä 0-1
var randomFloat = Float.random(in: 0...1)

print(randomNumber)
print(randomFloat)
```

Tämä voisi tuottaa esimerkiksi seuraavan tulosteen:

```
83
0.7324816
```

Syvemmälle aiheeseen:
Sattumanvaraisten numeroiden generoiminen on ollut osa ohjelmointia jo pitkään. Ennen Swiftiä siihen käytettiin usein C-kieleen kuuluvaa ```rand()``` -funktiota. Nykyään käytetään yleisesti algoritmeja, kuten Mersenne Twister, jotka pystyvät tuottamaan satunnaisia numerosekvenssejä ennustettavalla tavalla. Swiftissä käytetään tällaista algoritmia sisäisesti ```Int.random``` ja ```Float.random``` funktioissa.

Katso myös:
Jos haluat tietää lisää sattumanvaraisten numeroiden generoinnista, tässä on muutama hyödyllinen lähde:
- [Swiftin virallinen dokumentaatio sattumanvaraisista numeeroista](https://developer.apple.com/documentation/swift/Int/random/)
- [Kattava opas satunnaislukujen generoimiseen Swiftissä](https://www.hackingwithswift.com/example-code/system/generating-random-numbers-in-swift)
- [Mersenne Twister algoritmin esittely](https://en.wikipedia.org/wiki/Mersenne_Twister)