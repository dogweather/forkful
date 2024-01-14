---
title:                "Swift: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Monissa ohjelmointiprojekteissa on tarpeen muokata ja työstää merkkijonoja, ja usein tarvitaan tietoa merkkijonon pituudesta. On myös tärkeää ymmärtää, miten tieto tallennetaan erilaisiin muuttujiin, kuten merkkijonoon.

## Miten tehdä se

Merkkijonon pituuden selvittäminen Swiftillä on hyvin yksinkertaista. Kaikki merkkijonot ovat objekteja Swiftissä, ja niillä on oma `count`-metodi, joka palauttaa merkkijonon pituuden. Katso esimerkiksi:

```Swift
let sana = "Heippa!"
print(sana.count)
```

Tämän koodin tulostus olisi `7`, koska sana "Heippa!" koostuu seitsemästä merkistä. Voit myös käyttää `Character`-tyypin `count`-metodia, joka palauttaa merkkijonon pituuden merkkeinä sen sijaan, että laskisi kaikki välilyöntien ja erikoismerkkien määrän.

## Syvempi sukellus

Merkkijonon pituuden selvittäminen voi tuntua yksinkertaiselta, mutta se on tärkeä osa ohjelmoinnin perusteita. On tärkeää ymmärtää, että jokainen merkki merkkijonossa vie yhden muistipaikan, joten jos merkkijono on hyvin pitkä, se voi vaikuttaa suorituskykyyn.

Swiftissä on myös muita hyödyllisiä metodeja, kuten `isEmpty`, joka tarkistaa, onko merkkijono tyhjä, ja `hasPrefix` ja `hasSuffix`, jotka tarkistavat, alkaako tai päättyykö merkkijono tietyllä merkkijonolla. Opi lisää näistä Swiftin virallisesta dokumentaatiosta.

## Katso myös

- [Swift Programming Language Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer Documentation on Strings](https://developer.apple.com/documentation/swift/string)