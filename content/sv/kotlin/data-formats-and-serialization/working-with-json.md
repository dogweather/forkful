---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:14.746508-07:00
description: "Att arbeta med JSON (JavaScript Object Notation) i Kotlin inneb\xE4\
  r att tolka och generera JSON-data. Programmerare g\xF6r detta f\xF6r att enkelt\
  \ utbyta data\u2026"
lastmod: '2024-03-11T00:14:11.250507-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON (JavaScript Object Notation) i Kotlin inneb\xE4r att\
  \ tolka och generera JSON-data. Programmerare g\xF6r detta f\xF6r att enkelt utbyta\
  \ data\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON (JavaScript Object Notation) i Kotlin innebär att tolka och generera JSON-data. Programmerare gör detta för att enkelt utbyta data mellan olika lager i en applikation, eller kommunicera med webbtjänster, på grund av JSONs lätta och lättlästa format.

## Hur man gör:
Kotlin inkluderar inte inbyggt stöd för JSON men utnyttjar de kraftfulla funktionerna hos tredjepartsbibliotek såsom `Gson` av Google och `Kotlinx.serialization` av JetBrains. Här är hur du kan använda båda för att arbeta med JSON.

### Använda Gson
Lägg till Gson-beroendet i din `build.gradle`-fil:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Tolka JSON-sträng till ett objekt och tvärtom:
```kotlin
import com.google.gson.Gson

// Definiera en dataklass
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialisera
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Utdata: {"name":"John Doe","age":30}

    // Avserialisera
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Utdata: User(name=John Doe, age=30)
}
```

### Använda Kotlinx.serialization
Först, inkludera beroendet i din `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Därefter, tillämpa `kotlinx-serialization`-pluginet högst upp i ditt byggskript:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serialisera och avserialisera med Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Definiera en serialiserbar dataklass
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialisera
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Utdata: {"name":"Jane Doe","age":28}

    // Avserialisera
    val user = Json.decodeFromString<User>(json)
    println(user)  // Utdata: User(name=Jane Doe, age=28)
}
```

Både Gson och Kotlinx.serialization förenklar arbetet med JSON i Kotlin-applikationer, att välja den ena över den andra beror på dina specifika projektbehov och personliga preferenser.
