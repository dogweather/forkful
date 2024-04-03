---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:59.950411-07:00
description: "Hur man g\xF6r: Swift inkluderar inte inbyggt st\xF6d f\xF6r analys\
  \ och serialisering av YAML, vilket kr\xE4ver anv\xE4ndning av tredjepartskibliotek.\
  \ Ett popul\xE4rt val\u2026"
lastmod: '2024-03-13T22:44:38.271750-06:00'
model: gpt-4-0125-preview
summary: "Swift inkluderar inte inbyggt st\xF6d f\xF6r analys och serialisering av\
  \ YAML, vilket kr\xE4ver anv\xE4ndning av tredjepartskibliotek."
title: Att Arbeta med YAML
weight: 41
---

## Hur man gör:
Swift inkluderar inte inbyggt stöd för analys och serialisering av YAML, vilket kräver användning av tredjepartskibliotek. Ett populärt val är `Yams`, ett bibliotek för att arbeta med YAML i Swift.

Först måste du lägga till `Yams` i ditt projekt. Om du använder Swift Package Manager kan du lägga till det som ett beroende i din `Package.swift` fil:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Tolka YAML i Swift
Anta att du har följande YAML-konfiguration för en enkel app:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Så här kan du tolka denna YAML-sträng i Swift med hjälp av `Yams`:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

gör {
    om låt data = prova Yams.load(yaml: yamlString) som? [String: Any] {
        print(data)
        // Exempel på åtkomst till den tolkade datan
        om låt name = data["name"] som? String {
            print("Appnamn: \(name)")
        }
    }
} fånga {
    print("Fel vid tolkning av YAML: \(error)")
}
```

Exempelutdata:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Appnamn: MyApp
```

### Serialisera Swift-objekt till YAML
Att konvertera ett Swift-objekt tillbaka till en YAML-sträng är också enkelt med `Yams`. Anta att du har samma datastruktur som behöver serialiseras:

```swift
låt appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] som [String : Any]

gör {
    låt yamlString = prova Yams.dump(objekt: appInfo)
    print(yamlString)
} fånga {
    print("Fel vid serialisering till YAML: \(error)")
}
```

Detta kommer att producera en sträng formaterad som YAML:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Dessa exempel visar grundläggande operationer för att arbeta med YAML i Swift-applikationer. Kom ihåg, medan YAML utmärker sig i mänsklig läsbarhet och användarvänlighet, bör du alltid överväga de specifika behoven för din applikation, speciellt med avseende på prestanda och komplexitet, när du väljer ditt dataformat för serialisering.
