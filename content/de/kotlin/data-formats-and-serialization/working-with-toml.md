---
date: 2024-01-26 04:23:40.352463-07:00
description: "Wie zu: Um TOML in Kotlin zu handhaben, k\xF6nnte man eine Bibliothek\
  \ wie `ktoml` verwenden. Zuerst f\xFCgen wir die Abh\xE4ngigkeit in Ihre `build.gradle.kts`\u2026"
lastmod: '2024-03-13T22:44:53.869586-06:00'
model: gpt-4-0125-preview
summary: "Um TOML in Kotlin zu handhaben, k\xF6nnte man eine Bibliothek wie `ktoml`\
  \ verwenden."
title: Arbeiten mit TOML
weight: 39
---

## Wie zu:
Um TOML in Kotlin zu handhaben, könnte man eine Bibliothek wie `ktoml` verwenden. Zuerst fügen wir die Abhängigkeit in Ihre `build.gradle.kts` hinzu:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Jetzt lassen Sie uns etwas TOML parsen:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    der Host = databaseConfig.getString("host")
    der Port = databaseConfig.getLong("port")

    println("Datenbank-Host: $host")
    println("Datenbank-Port: $port")
}
```

Angenommen, `config.toml` sieht so aus:

```toml
[database]
host = "localhost"
port = 5432
```

Beispielausgabe wäre:

```
Datenbank-Host: localhost
Datenbank-Port: 5432
```

## Tiefergehend
TOML, ersonnen von GitHub-Mitbegründer Tom Preston-Werner im Jahr 2013, zielte darauf ab, unkomplizierter als YAML und typsicherer als JSON zu sein. Es wurde besonders mit `Cargo` von Rust und dem Modulsystem von Go ein Hit. Alternativen? YAML hat mehr Funktionen, JSON wird direkt in Objekte in vielen Programmiersprachen übersetzt, und dann gibt es immer noch das gute alte XML. Was die Implementierung betrifft, so ist ktoml, unter der Apache 2.0 Lizenz, eine reine Kotlin-Bibliothek und schleift keine Java-Bibliotheken mit sich, und bietet DSLs, um TOML zu schreiben, nicht nur zu lesen.

## Siehe auch
- Das TOML GitHub: https://github.com/toml-lang/toml
- Das ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
