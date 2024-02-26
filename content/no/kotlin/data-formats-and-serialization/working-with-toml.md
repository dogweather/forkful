---
date: 2024-01-26 04:24:28.389968-07:00
description: "TOML betyr Tom's Obvious, Minimal Language. Det brukes for konfigurasjonsfiler\
  \ fordi det er lett \xE5 lese og skrive for mennesker, samtidig som det er lett\u2026"
lastmod: '2024-02-25T18:49:38.952468-07:00'
model: gpt-4-0125-preview
summary: "TOML betyr Tom's Obvious, Minimal Language. Det brukes for konfigurasjonsfiler\
  \ fordi det er lett \xE5 lese og skrive for mennesker, samtidig som det er lett\u2026"
title: Jobbe med TOML
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML betyr Tom's Obvious, Minimal Language. Det brukes for konfigurasjonsfiler fordi det er lett å lese og skrive for mennesker, samtidig som det er lett å parse for maskiner. Utviklere velger TOML for å unngå rotet med XML og vanskelighetene med JSON når de jobber med konfigurasjoner.

## Hvordan:
For å håndtere TOML i Kotlin, kan du bruke et bibliotek som `ktoml`. Først, la oss legge til avhengigheten i din `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Nå, la oss parse noe TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlInnhold = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlInnhold.getTable("database")
    val vert = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("Database Vert: $vert")
    println("Database Port: $port")
}
```

Antatt at `config.toml` ser slik ut:

```toml
[database]
host = "localhost"
port = 5432
```

Eksempel på utdata vil være:

```
Database Vert: localhost
Database Port: 5432
```

## Dypdykk
TOML, laget av GitHub-medgrunnlegger Tom Preston-Werner i 2013, hadde som mål å være mer rettfram enn YAML og mer typesikkert enn JSON. Det har blitt en hit, spesielt med Rusts `Cargo` og Gos modulsystem. Alternativer? YAML har flere funksjoner, JSON oversettes direkte til objekter i mange programmeringsspråk, og så er det alltid gode, gamle XML. Når det gjelder implementasjon, er ktoml, under Apache 2.0-lisensen, et rent Kotlin-bibliotek og drar ikke med seg Java-bibliotek, og tilbyr DSL-er for å skrive TOML også, ikke bare lese.

## Se Også
- The TOML GitHub: https://github.com/toml-lang/toml
- The ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
