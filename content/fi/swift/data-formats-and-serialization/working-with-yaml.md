---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:54.766068-07:00
description: "YAML, joka tulee sanoista YAML Ain't Markup Language, on ihmisl\xE4\
  heinen tiedon sarjallistamisstandardi kaikille ohjelmointikielille. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.926936-06:00'
model: gpt-4-0125-preview
summary: "YAML, joka tulee sanoista YAML Ain't Markup Language, on ihmisl\xE4heinen\
  \ tiedon sarjallistamisstandardi kaikille ohjelmointikielille."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
Swift ei sisällä sisäänrakennettua tukea YAML:n jäsennykseen ja sarjallistamiseen, joten on tarpeen käyttää kolmannen osapuolen kirjastoja. Suosittu valinta on `Yams`, kirjasto YAML:n käsittelyyn Swiftissä.

Ensimmäiseksi sinun on lisättävä `Yams` projektiisi. Jos käytät Swift Package Manageria, voit lisätä sen riippuvuudeksi `Package.swift` tiedostoosi:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### YAML:n jäsennys Swiftiin
Oletetaan, että sinulla on seuraava YAML-konfiguraatio yksinkertaiselle sovellukselle:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Tässä on miten voit jäsentää tämän YAML-merkkijonon Swiftissä käyttäen `Yams`ia:

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
        // Esimerkki jäsennellyn datan käytöstä
        if let name = data["name"] as? String {
            print("Sovelluksen nimi: \(name)")
        }
    }
} catch {
    print("Virhe YAML:n jäsentämisessä: \(error)")
}
```

Näytekuloste:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Sovelluksen nimi: MyApp
```

### Swift-objektien sarjallistaminen YAML:ksi
Swift-objektin muuntaminen takaisin YAML-merkkijonoksi on myös suoraviivaista `Yams`in kanssa. Oletetaan, että sinulla on sama datarakenne, joka täytyy sarjallistaa:

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
    print("Virhe sarjallistettaessa YAML:ksi: \(error)")
}
```

Tämä tuottaa YAML-muotoillun merkkijonon:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Nämä esimerkit esittävät perustoimintoja YAML:n käsittelyssä Swift-sovelluksissa. Muista, että vaikka YAML erinomaisesti tukee ihmisen luettavuutta ja helppokäyttöisyyttä, aina kannattaa huomioida sovelluksesi erityistarpeet, erityisesti suorituskyvyn ja monimutkaisuuden osalta, kun valitset datan sarjallistamismuotoa.
