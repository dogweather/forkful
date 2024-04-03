---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:00.331541-07:00
description: "Wie geht das: Kotlin enth\xE4lt keine integrierte Unterst\xFCtzung f\xFC\
  r JSON, sondern nutzt die leistungsstarken Funktionen von Drittanbieter-Bibliotheken\
  \ wie\u2026"
lastmod: '2024-03-13T22:44:53.867510-06:00'
model: gpt-4-0125-preview
summary: "Kotlin enth\xE4lt keine integrierte Unterst\xFCtzung f\xFCr JSON, sondern\
  \ nutzt die leistungsstarken Funktionen von Drittanbieter-Bibliotheken wie `Gson`\
  \ von Google und `Kotlinx.serialization` von JetBrains."
title: Arbeiten mit JSON
weight: 38
---

## Wie geht das:
Kotlin enthält keine integrierte Unterstützung für JSON, sondern nutzt die leistungsstarken Funktionen von Drittanbieter-Bibliotheken wie `Gson` von Google und `Kotlinx.serialization` von JetBrains. Hier ist, wie Sie beide nutzen können, um mit JSON zu arbeiten.

### Verwendung von Gson
Fügen Sie die Gson-Abhängigkeit zu Ihrer `build.gradle`-Datei hinzu:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Ein JSON-String zu einem Objekt parsen und umgekehrt:
```kotlin
import com.google.gson.Gson

// Definiere eine Datenklasse
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialisieren
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Ausgabe: {"name":"John Doe","age":30}

    // Deserialisieren
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Ausgabe: User(name=John Doe, age=30)
}
```

### Verwendung von Kotlinx.serialization
Fügen Sie zunächst die Abhängigkeit in Ihre `build.gradle` ein:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Wenden Sie danach das `kotlinx-serialization`-Plugin an oberster Stelle Ihres Build-Skripts an:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serialisierung und Deserialisierung mit Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Definiere eine serialisierbare Datenklasse
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialisieren
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Ausgabe: {"name":"Jane Doe","age":28}

    // Deserialisieren
    val user = Json.decodeFromString<User>(json)
    println(user)  // Ausgabe: User(name=Jane Doe, age=28)
}
```

Sowohl Gson als auch Kotlinx.serialization vereinfachen die Arbeit mit JSON in Kotlin-Anwendungen. Die Wahl zwischen ihnen hängt von Ihren spezifischen Projektanforderungen und persönlichen Vorlieben ab.
