---
title:                "Swift: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on käytännöllinen tapa tallentaa tietoa tiedostoihin, jotka ovat helposti luettavissa sekä ihmisille että tietokoneille. Se on erityisen hyödyllinen esimerkiksi sovellusten konfiguraatiotiedostojen käsittelyssä. 

## Miten

YAML:in käyttäminen Swiftissä on helppoa ja nopeaa. Yksi tapa on käyttää YAMLSwift kirjastoa, joka on saatavilla Swift Package Managerin kautta. Seuraavassa on esimerkki, miten voit lukea ja tulostaa YAML-tiedoston sisältöä:

```Swift
let yamlString = """
name: John Smith
occupation: Developer
languages:
  - Swift
  - JavaScript
"""

do {
  let yamlValues = try YAMLDecoder().decode(YAML.self, from: yamlString)
  print(yamlValues) // {name: John Smith, occupation: Developer, languages: [Swift, JavaScript]}
} catch {
  print(error)
}
```

Voit myös luoda oman YAML-tiedoston ja tallentaa siihen tietoa seuraavasti:

```Swift
struct Person: Codable {
  var name: String
  var occupation: String
  var languages: [String]
}

let john = Person(name: "John Smith", occupation: "Developer", languages: ["Swift", "JavaScript"])

do {
  let yamlString = try YAMLEncoder().encode(john)
  print(yamlString) // name: John Smith, occupation: Developer, languages: [Swift, JavaScript]
  try yamlString.write(to: "person.yaml", atomically: true, encoding: .utf8)
} catch {
  print(error)
}
```

## Syväsukellus

YAML tarjoaa monipuolisia mahdollisuuksia tietojen tallentamiseen. Voit esimerkiksi luoda monimutkaisempia rakenteita, kuten listoja ja karttoja, tai käyttää merkkejä ja kommentteja selkeyttämään tiedoston sisältöä. Lisäksi voit käyttää erilaisia tapoja käsitellä tiedostoja, kuten automaattinen muunnos JSON-muotoon ja muokattavissa olevat asetukset.

## Katso myös

- YAMLSwift kirjasto: https://github.com/jpsim/YAMLSwift
- Virallinen YAML-sivusto: https://yaml.org/
- Swift Package Manager: https://swift.org/package-manager/