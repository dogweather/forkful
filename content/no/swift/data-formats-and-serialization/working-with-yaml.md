---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:57.186133-07:00
description: "Hvordan: Swift inkluderer ikke innebygd st\xF8tte for YAML-parsing og\
  \ -serialisering, noe som n\xF8dvendiggj\xF8r bruk av tredjepartsbiblioteker. Et\
  \ popul\xE6rt valg\u2026"
lastmod: '2024-03-13T22:44:41.162152-06:00'
model: gpt-4-0125-preview
summary: "Swift inkluderer ikke innebygd st\xF8tte for YAML-parsing og -serialisering,\
  \ noe som n\xF8dvendiggj\xF8r bruk av tredjepartsbiblioteker."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Swift inkluderer ikke innebygd støtte for YAML-parsing og -serialisering, noe som nødvendiggjør bruk av tredjepartsbiblioteker. Et populært valg er `Yams`, et bibliotek for arbeid med YAML i Swift.

Først må du legge til `Yams` i prosjektet ditt. Hvis du bruker Swift Package Manager, kan du legge den til som en avhengighet i din `Package.swift`-fil:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Parse YAML til Swift
Anta at du har følgende YAML-konfigurasjon for en enkel app:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Her er hvordan du kan parse denne YAML-strengen i Swift ved hjelp av `Yams`:

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

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Eksempel på tilgang til den parsete dataen
        if let name = data["name"] as? String {
            print("Appnavn: \(name)")
        }
    }
} catch {
    print("Feil ved parsing av YAML: \(error)")
}
```

Eksempel på utskrift:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Appnavn: MyApp
```

### Serialisere Swift-objekter til YAML
Å konvertere et Swift-objekt tilbake til en YAML-streng er også enkelt med `Yams`. Anta at du har den samme datastrukturen som trenger å bli serialisert:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("Feil ved serialisering til YAML: \(error)")
}
```

Dette vil produsere en YAML-formattert streng:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Disse eksemplene demonstrerer grunnleggende operasjoner for å jobbe med YAML i Swift-applikasjoner. Husk, selv om YAML utmerker seg i menneskelig lesbarhet og brukervennlighet, alltid vurdér de spesifikke behovene til applikasjonen din, spesielt med hensyn til ytelse og kompleksitet, når du velger ditt data-serieringsformat.
