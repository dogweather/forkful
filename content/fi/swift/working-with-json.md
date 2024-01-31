---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) on kevyt dataformaatti datan tallennukseen ja siirtoon. Ohjelmoijat käyttävät JSONia sen yksinkertaisuuden ja ihmisen lukevan muodon vuoksi, ja se toimii hyvin eri alustojen ja kielten välillä.

## How to:
Swift käsittelee JSONia Codable-protokollan ja JSONDecoder-kirjaston avulla. Data muunnetaan `Encodable` -protokollaa toteuttavasta Swifin tyyppistä JSON:ksi ja päinvastoin. Katso esimerkki:

```Swift
import Foundation

// Malli, joka edustaa JSON-tietoa
struct Käyttäjä: Codable {
    var nimi: String
    var ikä: Int
}

// JSON merkkijono, jota käsitellään
let jsonMerkkijono = """
{
    "nimi": "Matti",
    "ikä": 28
}
""".data(using: .utf8)!

// JSON datan muuntaminen Swift-olioksi
do {
    let dekoodattuKäyttäjä = try JSONDecoder().decode(Käyttäjä.self, from: jsonMerkkijono)
    print(dekoodattuKäyttäjä) // Tulostaa Käyttäjän tiedot
} catch {
    print(error)
}
```

## Deep Dive
JSON-muoto esitettiin vuonna 2001 ja se nousi nopeasti suosioon keveytensä ja helppokäyttöisyytensä vuoksi. Vaihtoehtoisia formaatteja ovat XML ja YAML. Swiftin `Codable`-protokolla, joka julkistettiin Swift 4:ssä, tekee JSONin käsittelystä helpompaa automatisoimalla monet siihen liittyvät rutiinit.

## See Also
- Swiftin virallinen dokumentaatio Codable-protokollasta: [Swift Codable Documentation](https://developer.apple.com/documentation/swift/codable)
- JSON-syntaksin selitys: [JSON.org](https://www.json.org/json-en.html)
- Apple Developer -artikkeli JSONista ja Swiftistä: [Working with JSON in Swift](https://developer.apple.com/swift/blog/?id=37)
