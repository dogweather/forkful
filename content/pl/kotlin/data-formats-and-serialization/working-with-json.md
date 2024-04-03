---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:10.890579-07:00
description: "Praca z JSON-em (JavaScript Object Notation) w Kotlinie obejmuje analizowanie\
  \ i generowanie danych JSON. Programi\u015Bci robi\u0105 to, aby \u0142atwo wymienia\u0107\
  \ dane\u2026"
lastmod: '2024-03-13T22:44:35.386587-06:00'
model: gpt-4-0125-preview
summary: Praca z JSON-em (JavaScript Object Notation) w Kotlinie obejmuje analizowanie
  i generowanie danych JSON.
title: Praca z JSON
weight: 38
---

## Co i dlaczego?
Praca z JSON-em (JavaScript Object Notation) w Kotlinie obejmuje analizowanie i generowanie danych JSON. Programiści robią to, aby łatwo wymieniać dane między różnymi warstwami aplikacji lub komunikować się z usługami sieciowymi, ze względu na lekki i czytelny format JSON.

## Jak to zrobić:
Kotlin nie zawiera wbudowanego wsparcia dla JSON, ale wykorzystuje potężne funkcje bibliotek stron trzecich, takich jak `Gson` od Google i `Kotlinx.serialization` od JetBrains. Oto jak możesz używać obu do pracy z JSON.

### Korzystanie z Gson
Dodaj zależność Gson do swojego pliku `build.gradle`:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Parsowanie łańcucha JSON na obiekt i odwrotnie:
```kotlin
import com.google.gson.Gson

// Zdefiniuj klasę danych
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serializacja
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Wynik: {"name":"John Doe","age":30}

    // Deserializacja
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Wynik: User(name=John Doe, age=30)
}
```

### Korzystanie z Kotlinx.serialization
Najpierw dołącz zależność w swoim `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Następnie zastosuj wtyczkę `kotlinx-serialization` na początku skryptu budującego:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serializacja i deserializacja z Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Zdefiniuj serializowalną klasę danych
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serializacja
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Wynik: {"name":"Jane Doe","age":28}

    // Deserializacja
    val user = Json.decodeFromString<User>(json)
    println(user)  // Wynik: User(name=Jane Doe, age=28)
}
```

Zarówno Gson, jak i Kotlinx.serialization upraszczają pracę z JSON w aplikacjach Kotlin. Wybór jednej z tych opcji zależy od konkretnych wymagań projektu i osobistych preferencji.
