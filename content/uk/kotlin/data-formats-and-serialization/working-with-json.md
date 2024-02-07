---
title:                "Робота з JSON"
date:                  2024-02-03T19:23:42.714014-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
