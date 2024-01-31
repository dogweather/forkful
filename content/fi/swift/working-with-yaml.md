---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"

category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi YAMLilla työskentely? YAML on dataformaatti, joka on helppo lukea ja kirjoittaa ohjelmoijille sekä koneille. Sen avulla hallitaan konfiguraatiotietoja ja dataa, erityisesti monimutkaisemmissa sovelluksissa ja pilvipalveluissa.

## How to:
Swift-koodilla YAMLin käsittelyyn tarvitset ulkopuolisen kirjaston, kuten Yams. Tässä esimerkit lukemisesta ja kirjoittamisesta.

```Swift
import Yams

let yaml = """
name: Esimerkki
age: 30
languages:
  - Swift
  - Python
  - JavaScript
"""

// YAMLin lukeminen
do {
    if let data = try Yams.load(yaml: yaml) as? [String: Any] {
        print(data["name"] ?? "Nimi puuttuu")
    }
} catch {
    print("Virhe YAMLin latauksessa: \(error)")
}

// YAMLin kirjoittaminen
let personData = ["name": "Toinen Esimerkki", "age": 25, "languages": ["Swift", "Java"]]
do {
    let newYaml = try Yams.dump(object: personData)
    print(newYaml)
} catch {
    print("Virhe YAMLin kirjoittamisessa: \(error)")
}
```

Tuloste lukemiselle olisi `Esimerkki` ja kirjoittamiselle uusi YAML-muotoinen merkkijono.

## Deep Dive
YAML on lyhenne sanoista "YAML Ain't Markup Language" ja se on alun perin luotu 2001. Se on vähemmän monimutkainen kuin XML ja usein käytetään JSONin vaihtoehtona, koska se on luettavampi. Swiftissä ei ole sisäänrakennettua tukea YAMLille, joten Yams-kirjaston kaltainen työkalu tarvitaan.

## See Also
- Yams GitHub-sivu: https://github.com/jpsim/Yams
- YAMLin virallinen sivusto: https://yaml.org
- Swift Package Managerin dokumentaatio: https://swift.org/package-manager/
