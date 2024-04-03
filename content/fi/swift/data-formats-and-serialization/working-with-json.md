---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:14.636963-07:00
description: "JSON:n k\xE4ytt\xE4minen Swiftiss\xE4 tarkoittaa kevyen datanvaihtoformaatin\
  \ k\xE4sittely\xE4. Ohjelmoijat k\xE4ytt\xE4v\xE4t JSONia datan siirt\xE4miseen\
  \ palvelimen ja web-\u2026"
lastmod: '2024-03-13T22:44:56.928421-06:00'
model: gpt-4-0125-preview
summary: "JSON:n k\xE4ytt\xE4minen Swiftiss\xE4 tarkoittaa kevyen datanvaihtoformaatin\
  \ k\xE4sittely\xE4."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Mikä & Miksi?

JSON:n käyttäminen Swiftissä tarkoittaa kevyen datanvaihtoformaatin käsittelyä. Ohjelmoijat käyttävät JSONia datan siirtämiseen palvelimen ja web-sovelluksen välillä, koska se on luettavissa ja helppo jäsentää sekä ihmisille että koneille.

## Kuinka:

Swift tekee JSONin jäsentämisestä suoraviivaista `Codable`-protokollan avulla. Tässä on, kuinka dekoodaat JSONin Swift-objektiksi:

```Swift
import Foundation

// Määrittele malli, joka noudattaa Codablea
struct User: Codable {
    var name: String
    var age: Int
}

// JSON merkkijono
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// Muunna JSON merkkijono Dataksi
if let jsonData = jsonString.data(using: .utf8) {
    // Dekoodaa JSON data User-objektiksi
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Nimi: \(user.name), Ikä: \(user.age)")
    } catch {
        print("Virhe JSONin dekoodauksessa: \(error)")
    }
}
```

Esimerkkituloste:
```
Nimi: John Doe, Ikä: 30
```

## Syväsukellus

JSON (JavaScript Object Notation) on ollut laajalti käytössä 2000-luvun alusta lähtien, kun Douglas Crockford määritteli sen. Se korvasi XML:n monissa käyttötapauksissa sen yksinkertaisemman syntaksin ja paremman suorituskyvyn vuoksi. Vaikka Swiftin `Codable` on mennä JSONille, vaihtoehtoja, kuten `JSONSerialization`, on olemassa, kun käsitellään ei-Codable-yhteensopivia tyyppejä. Kulissien takana `Codable` abstrahoi alemman tason jäsentämisen ja tekee serialisoinnin/deserialisoinnin saumattomaksi.

## Katso Myös

- Tutustu lisää JSONiin ja Swiftiin virallisessa Swift-blogissa: [Swift.org](https://swift.org/blog/)
- Tutustu `Codable`-dokumentaatioon: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Monimutkaisia JSON-rakenteita varten harkitse kolmannen osapuolen kirjastoja, kuten SwiftyJSON, saatavilla [GitHubissa](https://github.com/SwiftyJSON/SwiftyJSON).
