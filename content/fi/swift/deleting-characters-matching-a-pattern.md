---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
date:                  2024-01-20T17:43:14.076075-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? / Mitä & Miksi?
Kun puhutaan merkkien poistamisesta tietyllä kuvioinnilla, tarkoitetaan prosessia, jossa valikoidut merkit siivotaan pois merkkijonosta. Ohjelmoijat tekevät tätä dataa siistiessään, tarpeettoman tiedon karsimiseksi tai datan muotoa muuttaakseen.

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
