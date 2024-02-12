---
title:                "Työskentely YAML:n kanssa"
aliases:
- /fi/swift/working-with-yaml.md
date:                  2024-02-03T19:26:54.766068-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
YAML, joka tulee sanoista YAML Ain't Markup Language, on ihmisläheinen tiedon sarjallistamisstandardi kaikille ohjelmointikielille. Ohjelmoijat käyttävät sitä konfiguraatiotiedostoihin, prosessien väliseen viestintään ja datan tallennukseen, koska sen luettavuus on huomattavasti lähempänä tavallista englantia verrattuna muihin datamuotoihin kuten XML tai JSON, mikä tekee siitä yksinkertaisemman ymmärtää ja kirjoittaa.

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
