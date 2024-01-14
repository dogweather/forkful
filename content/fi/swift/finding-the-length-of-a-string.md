---
title:    "Swift: Merkkijonon pituuden löytäminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat selvittää merkkijonon pituuden Swift-ohjelmassa. Kenties haluat tarkistaa, onko merkkijono riittävän lyhyt tietylle tietokannalle tai rajapinnalle. Tai ehkä haluat vain tarkistaa, onko käyttäjän syöte sopiva ennen sen tallentamista.

## Kuinka

Voit selvittää merkkijonon pituuden käyttämällä String-tyypin length-attribuuttia. Katso alla olevaa koodia ja tulosteita nähdäksesi miten tämä toimii käytännössä:

```Swift
let sana = "Hei maailma!"
print(sana.length)
// Tulostaa: 12

let tyhja = ""
print(tyhja.length)
// Tulostaa: 0
```

## Syvemmälle

Merkkijonon pituuden selvittäminen perustuu Unicode-yhdistimiin. Unicode-yhdistin on yksi tai useampi numeromuodoinen arvo, joka edustaa merkkiä merkistössä. Esimerkiksi "a" merkki on yhdistelmästä U+0061, eli numeromuotoisena 97. Nämä numeromuotoiset arvot lasketaan kokonaispituudeksi, kun haluat selvittää merkkijonon pituuden.

## Katso myös

- [Swiftin virallinen dokumentaatio String-tyypistä (englanniksi)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Stack Overflow: How to get length of String in Swift (englanniksi)](https://stackoverflow.com/questions/24041993/how-do-i-get-the-length-of-a-string-in-swift)