---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:02.639712-07:00
description: "TOML staat voor Tom's Obvious, Minimal Language. Het wordt gebruikt\
  \ voor configuratiebestanden omdat het makkelijk te lezen en te schrijven is voor\u2026"
lastmod: '2024-03-11T00:14:24.615136-06:00'
model: gpt-4-0125-preview
summary: "TOML staat voor Tom's Obvious, Minimal Language. Het wordt gebruikt voor\
  \ configuratiebestanden omdat het makkelijk te lezen en te schrijven is voor\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML staat voor Tom's Obvious, Minimal Language. Het wordt gebruikt voor configuratiebestanden omdat het makkelijk te lezen en te schrijven is voor mensen, terwijl het tegelijkertijd eenvoudig te parsen is voor machines. Ontwikkelaars grijpen naar TOML om de rommel van XML en de listigheid van JSON te vermijden wanneer ze configuraties aan het slingeren zijn.

## Hoe te:
Om TOML in Kotlin te hanteren, zou je een bibliotheek zoals `ktoml` kunnen gebruiken. Laten we eerst de afhankelijkheid toevoegen in je `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Laten we nu wat TOML parsen:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlInhoud = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlInhoud.getTable("database")
    de host = databaseConfig.getString("host")
    de poort = databaseConfig.getLong("port")

    println("Database Host: $host")
    println("Database Poort: $poort")
}
```

Aangenomen dat `config.toml` er zo uitziet:

```toml
[database]
host = "localhost"
port = 5432
```

Voorbeelduitvoer zou zijn:

```
Database Host: localhost
Database Poort: 5432
```

## Diepgaand
TOML, bedacht door GitHub mede-oprichter Tom Preston-Werner in 2013, had als doel eenvoudiger te zijn dan YAML en meer type-veilig dan JSON. Het is een hit geworden, vooral met Rust's `Cargo` en het modulesysteem van Go. Alternatieven? YAML heeft meer functies, JSON vertaalt direct naar objecten in veel programmeertalen, en er is altijd het goede oude XML. Wat implementatie betreft, ktoml, onder de Apache 2.0 licentie, is een pure Kotlin bibliotheek en sleept geen Java libs mee, en biedt DSLs om TOML te schrijven, niet alleen te lezen.

## Zie Ook
- De TOML GitHub: https://github.com/toml-lang/toml
- De ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
