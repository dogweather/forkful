---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Swift: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden löytäminen on yksinkertainen mutta tärkeä ohjelmointikäytäntö, jota käytetään usein Swiftissä. Se tarkoittaa yksinkertaisesti merkkijonon sisältämien merkkien määrän laskemista. Tämä on hyödyllistä esimerkiksi merkkijonojen käsittelyssä ja tarkistamisessa.

## Miten:
```Swift
let text = "Tervehdys!"
print(text.count) // Tulostaa 10
```

Yllä oleva esimerkki käyttää Swiftin `count`-ominaisuutta laskeakseen merkkijonon pituuden. Se toimii laskemalla kaikki merkit merkkijonosta ja palauttamalla niiden määrän. Voit myös käyttää `String`-tyypin `count`-metodia samalla tavalla.

## Syväsukellus:
Merkkijonojen pituuden laskeminen on ollut olennainen osa ohjelmointia jo pitkään. Aikaisemmin tämä tehtiin usein manuaalisesti laskemalla merkkien määrää merkkijonosta. Nykypäivän ohjelmointikielissä, kuten Swiftissä, on kuitenkin sisäänrakennettuja ominaisuuksia tähän tarkoitukseen.

Jos haluat löytää merkkijonon pituuden ilman näitä sisäänrakennettuja ominaisuuksia, voit käyttää myös `index(startIndex, offsetBy: String.IndexDistance)` -metodia. Tämä metodi palauttaa annetun määrän metkkiä alkuperäisen merkkijonon alusta ja laskee niiden määrän.

## Katso myös:
- [Swiftin virallinen dokumentaatio String-tyypin `count`-ominaisuudesta](https://developer.apple.com/documentation/swift/string)
- [Stack Overflow - Englanninkielinen keskusteluketju merkkijonon pituuden laskemisesta Swiftissä](https://stackoverflow.com/questions/24050634/counting-length-of-a-string-in-swift)