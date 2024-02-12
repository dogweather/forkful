---
title:                "Arbeiten mit JSON"
aliases:
- de/kotlin/working-with-json.md
date:                  2024-02-03T19:23:00.331541-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit JSON (JavaScript Object Notation) in Kotlin umfasst das Parsen und Generieren von JSON-Daten. Programmierer tun dies, um Daten leicht zwischen verschiedenen Schichten in einer Anwendung auszutauschen oder mit Webdiensten zu kommunizieren, aufgrund des leichtgewichtigen und für Menschen lesbaren Formats von JSON.

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
