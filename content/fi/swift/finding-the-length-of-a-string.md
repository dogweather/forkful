---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:48:29.771155-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
"Mikä & Miksi?"
Stringin pituuden selvittäminen tarkoittaa merkkien määrän laskemista siinä. Koodarit tekevät tämän validoidakseen syötteen, rajatakseen tekstikenttiä, tai missä tahansa, missä on tarve tietää, minkä verran dataa käsitellään.

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