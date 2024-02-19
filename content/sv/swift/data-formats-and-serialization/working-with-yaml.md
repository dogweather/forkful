---
aliases:
- /sv/swift/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:59.950411-07:00
description: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r en standard\
  \ f\xF6r serialisering av data som \xE4r l\xE4tt f\xF6r m\xE4nniskor att l\xE4sa,\
  \ och som fungerar med alla\u2026"
lastmod: 2024-02-18 23:08:52.143843
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r en standard f\xF6\
  r serialisering av data som \xE4r l\xE4tt f\xF6r m\xE4nniskor att l\xE4sa, och som\
  \ fungerar med alla\u2026"
title: Att Arbeta med YAML
---

{{< edit_this_page >}}

## Vad och Varför?
YAML, som står för YAML Ain't Markup Language, är en standard för serialisering av data som är lätt för människor att läsa, och som fungerar med alla programmeringsspråk. Programmerare använder det för konfigurationsfiler, meddelanden mellan processer och datalagring eftersom dess läsbarhet ligger mycket närmare vanlig engelska jämfört med andra dataformat som XML eller JSON, vilket gör det enklare att förstå och skriva.

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
