---
aliases:
- /uk/kotlin/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:42.714014-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0443 Kotlin \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\
  \u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433 \u0442\u0430 \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0456\u044E \u0434\u0430\u043D\u0438\u0445 JSON. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u043B\u0435\u0433\u043A\u043E\
  \u0433\u043E \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\
  \u0438 \u043C\u0456\u0436\u2026"
lastmod: 2024-02-18 23:09:00.304907
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0443 Kotlin \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u043F\
  \u0430\u0440\u0441\u0438\u043D\u0433 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0430\u0446\u0456\u044E \u0434\u0430\u043D\u0438\u0445 JSON. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435 \u0434\u043B\u044F \u043B\u0435\u0433\u043A\u043E\u0433\u043E\
  \ \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438 \u043C\
  \u0456\u0436\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
---

{{< edit_this_page >}}

## Що і чому?
Робота з JSON (JavaScript Object Notation) у Kotlin передбачає парсинг та генерацію даних JSON. Програмісти роблять це для легкого обміну даними між різними шарами у програмі або для комунікації з веб-сервісами, завдяки легковісному та зрозумілому формату JSON.

## Як:
Kotlin не має вбудованої підтримки для JSON, але використовує потужні функції сторонніх бібліотек, таких як `Gson` від Google та `Kotlinx.serialization` від JetBrains. Ось як ви можете використовувати обидві для роботи з JSON.

### Використання Gson
Додайте залежність Gson до вашого файлу `build.gradle`:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Парсинг рядка JSON в об'єкт і навпаки:
```kotlin
import com.google.gson.Gson

// Оголосити data клас
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Серіалізація
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Вивід: {"name":"John Doe","age":30}

    // Десеріалізація
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Вивід: User(name=John Doe, age=30)
}
```

### Використання Kotlinx.serialization
Спочатку, додайте залежність у ваш `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Після цього, застосуйте плагін `kotlinx-serialization` на початку вашого скріпта збірки:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Серіалізація і десеріалізація з Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Оголосити серіалізований data клас
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Серіалізація
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Вивід: {"name":"Jane Doe","age":28}

    // Десеріалізація
    val user = Json.decodeFromString<User>(json)
    println(user)  // Вивід: User(name=Jane Doe, age=28)
}
```

Як Gson, так і Kotlinx.serialization спрощують роботу з JSON у програмах Kotlin, вибір одного над іншим залежить від конкретних вимог вашого проекту та особистих переваг.
