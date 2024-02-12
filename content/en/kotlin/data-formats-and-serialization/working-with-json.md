---
title:                "Working with JSON"
aliases:
- /en/kotlin/working-with-json.md
date:                  2024-02-03T19:03:16.078931-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON (JavaScript Object Notation) in Kotlin involves parsing and generating JSON data. Programmers do this to easily exchange data between different layers in an application, or communicate with web services, due to JSON's lightweight and human-readable format.

## How to:
Kotlin does not include built-in support for JSON but leverages the powerful features of third-party libraries such as `Gson` by Google and `Kotlinx.serialization` by JetBrains. Here’s how you can use both to work with JSON.

### Using Gson
Add the Gson dependency to your `build.gradle` file:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Parsing JSON string to an object and vice versa:
```kotlin
import com.google.gson.Gson

// Define a data class
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialize
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Output: {"name":"John Doe","age":30}

    // Deserialize
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Output: User(name=John Doe, age=30)
}
```

### Using Kotlinx.serialization
First, include the dependency in your `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Afterward, apply the `kotlinx-serialization` plugin at the top of your build script:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serializing and deserializing with Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Define a serializable data class
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialize
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Output: {"name":"Jane Doe","age":28}

    // Deserialize
    val user = Json.decodeFromString<User>(json)
    println(user)  // Output: User(name=Jane Doe, age=28)
}
```

Both Gson and Kotlinx.serialization simplify working with JSON in Kotlin applications, choosing one over the other depends on your specific project requirements and personal preferences.
