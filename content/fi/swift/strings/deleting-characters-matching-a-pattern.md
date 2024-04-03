---
date: 2024-01-20 17:43:14.076075-07:00
description: "How to: / Kuinka: Swiftiss\xE4 merkkien poistaminen kuviota k\xE4ytt\xE4\
  en onnistuu `String` laajennoksilla ja s\xE4\xE4nn\xF6llisill\xE4 lausekkeilla (regexp).\
  \ T\xE4ss\xE4 helppo\u2026"
lastmod: '2024-03-13T22:44:56.891912-06:00'
model: gpt-4-1106-preview
summary: "Swiftiss\xE4 merkkien poistaminen kuviota k\xE4ytt\xE4en onnistuu `String`\
  \ laajennoksilla ja s\xE4\xE4nn\xF6llisill\xE4 lausekkeilla (regexp)."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: / Kuinka:
Swiftissä merkkien poistaminen kuviota käyttäen onnistuu `String` laajennoksilla ja säännöllisillä lausekkeilla (regexp). Tässä helppo esimerkki:

```Swift
extension String {
    func deleteCharacters(matching pattern: String) -> String {
        return self.replacingOccurrences(of: pattern, with: "", options: .regularExpression)
    }
}

let sampleString = "Tämä1 on2 testi3merkkijono4!"
let cleanedString = sampleString.deleteCharacters(matching: "\\d") // Poistaa kaikki numerot
print(cleanedString) // Tulostaa: "Tämä on testimerkkijono!"
```

## Deep Dive / Sukellus syvyyksiin:
Swift otti käyttöön säännölliset lausekkeet jonkun aikaa sitten, ja ne ovat siitä lähtien olleet vahva väline merkkijonojen käsittelyyn. Vaihtoehtona on käyttää myös `NSPredicate`, mutta se on raskaampi ja monimutkaisempi tapa. Poistotapahtuman tehokkuus riippuu kuvion monimutkaisuudesta ja stringin pituudesta. Siinä missä yksinkertaiset toimenpiteet suoriutuvat nopeasti, monimutkaisemmat regexpit voivat hidastaa suorituskykyä.

## See Also / Katso myös:
- Swift-kielen virallinen dokumentaatio `String`-tyypistä: [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- Säännöllisten lausekkeiden käytöstä Swiftissä: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Online säännöllisten lausekkeiden testaustyökalu: [RegExr](https://regexr.com/)
