---
title:                "Werken met YAML"
aliases:
- nl/swift/working-with-yaml.md
date:                  2024-01-28T22:11:54.860071-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
YAML, een afkorting voor "YAML Ain't Markup Language", is een voor mensen leesbare gegevensserialisatiestandaard die we kunnen gebruiken om bestanden te configureren of gegevensuitwisseling te doen. Programmeurs zijn dol op YAML vanwege de eenvoud en leesbaarheid, vooral in configuratie-instellingen, CI/CD-scripts en containers orchestratiesystemen.

## Hoe:
Swift kan standaard geen YAML verwerken, dus we moeten een externe bibliotheek zoals Yams gebruiken. Voeg eerst Yams toe aan je `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", vanaf: "4.0.0")
]
```

Importeer vervolgens Yams en gebruik het om YAML naar een Swift-woordenboek te parseren:

```swift
import Yams

let yamlString = """
name: John Doe
age: 34
languages:
  - Swift
  - Python
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
    }
} catch {
    print("Mislukt om YAML-string te parseren.")
}

// Uitvoer:
// ["name": "John Doe", "age": 34, "languages": ["Swift", "Python"]]
```

Als je YAML wilt genereren vanuit Swift-objecten:

```swift
import Yams

let woordenboek: [String: Any] = [
    "name": "Jane Smith",
    "age": 28,
    "languages": ["Java", "Kotlin"]
]

do {
    let yaml = try Yams.dump(object: woordenboek)
    print(yaml)
} catch {
    print("Mislukt om woordenboek naar YAML te converteren.")
}

// Uitvoer:
// age: 28
// languages:
//   - Java
//   - Kotlin
// name: Jane Smith
```

## Diepgaand
YAML is in 2001 ontstaan als een mensvriendelijk alternatief voor XML. Het lijkt op JSON met minder gebruik van haakjes en betere leesbaarheid voor mensen. Hoewel JSON de voorkeur heeft voor web-API's, heeft YAML de voorkeur voor configuratiebestanden. Alternatieven zijn onder andere TOML en JSON5, maar YAML's gebruik van witruimte en de mogelijkheid om regels te becommentariëren maken het wenselijk. Met Yams benadert Swift YAML-verwerking met class mapping, en biedt een balans tussen scriptachtige eenvoud en typeveiligheid.

## Zie ook
- Officiële YAML-site voor specificatiedetails: [https://yaml.org](https://yaml.org)
- Yams GitHub-repository: [https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- Documentatie van Swift Package Manager: [https://swift.org/package-manager/](https://swift.org/package-manager/)
