---
date: 2024-01-20 17:48:29.771155-07:00
description: "How to: \"N\xE4in tehd\xE4\xE4n:\" Swiftiss\xE4 voit selvitt\xE4\xE4\
  \ stringin pituuden k\xE4ytt\xE4m\xE4ll\xE4 `count` ominaisuutta. Kataotaanpa esimerkki."
lastmod: '2024-04-05T21:53:58.477490-06:00'
model: gpt-4-1106-preview
summary: "\"N\xE4in tehd\xE4\xE4n:\" Swiftiss\xE4 voit selvitt\xE4\xE4 stringin pituuden\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `count` ominaisuutta."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to:
"Näin tehdään:"
Swiftissä voit selvittää stringin pituuden käyttämällä `count` ominaisuutta. Kataotaanpa esimerkki:

```Swift
let greeting = "Moi maailma"
let length = greeting.count
print("Stringin '\(greeting)' pituus on \(length).")
```

Lähtö tulisi olema: `Stringin 'Moi maailma' pituus on 11.`

Jos tarvitset pituuden ilman välilyöntejä, voit poistaa ne ennen laskemista:

```Swift
let trimmedLength = greeting.replacingOccurrences(of: " ", with: "").count
print("Stringin '\(greeting)' pituus ilman välilyöntejä on \(trimmedLength).")
```

Tulostuu: `Stringin 'Moi maailma' pituus ilman välilyöntejä on 10.`

## Deep Dive
"Syväkurkistus":
Historiallisesti stringin pituuden laskeminen on vaihdellut kielittäin. Joissain kielissä se on monimutkaisempaa erikoismerkkien tai koodausten takia. Swift käyttää Unicode-skalareita, mikä tarkoittaa, että se laskee 'Character' -tyyppisten merkkien määrän, jotka voivat koostua useammasta kuin yhdestä Unicode-skaarasta.

Vaihtoehtoisesti, voit käyttää `utf8`, `utf16` tai `unicodeScalars` ominaisuuksia, jos tarvitset tietyn koodauksen mukaisen pituuden:

```Swift
let utf8Length = greeting.utf8.count
let utf16Length = greeting.utf16.count
let unicodeScalarsLength = greeting.unicodeScalars.count
```

Nämä voivat olla hyödyllisiä, jos käsittelet matalan tason string-manipulaatiota tai kommunikoit järjestelmien välillä, jotka edellyttävät tietynlaista koodausta.

## See Also
"Katso Myös":
- Swift dokumentaatio `String`-luokasta: [Apple String Documentation](https://developer.apple.com/documentation/swift/string)
- Unicode standardi: [Unicode.org](https://www.unicode.org)
- Swift Standard Library:n merkkien laskemistyökalut: [Apple Character Documentation](https://developer.apple.com/documentation/swift/character)
