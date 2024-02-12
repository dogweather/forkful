---
title:                "Trabajando con JSON"
aliases: - /es/kotlin/working-with-json.md
date:                  2024-02-03T19:22:51.370470-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con JSON (JavaScript Object Notation) en Kotlin implica analizar y generar datos JSON. Los programadores hacen esto para intercambiar datos fácilmente entre diferentes capas en una aplicación o comunicarse con servicios web, debido al formato ligero y legible por humanos de JSON.

## Cómo hacerlo:
Kotlin no incluye soporte integrado para JSON, pero aprovecha las poderosas características de bibliotecas de terceros como `Gson` de Google y `Kotlinx.serialization` de JetBrains. Aquí te mostramos cómo puedes usar ambas para trabajar con JSON.

### Usando Gson
Agrega la dependencia de Gson a tu archivo `build.gradle`:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Analizando un string JSON a un objeto y viceversa:
```kotlin
import com.google.gson.Gson

// Definir una clase de datos
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serializar
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Salida: {"name":"John Doe","age":30}

    // Deserializar
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Salida: User(name=John Doe, age=30)
}
```

### Usando Kotlinx.serialization
Primero, incluye la dependencia en tu `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Después, aplica el plugin `kotlinx-serialization` en la parte superior de tu script de construcción:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serializando y deserializando con Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Definir una clase de datos serializable
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serializar
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Salida: {"name":"Jane Doe","age":28}

    // Deserializar
    val user = Json.decodeFromString<User>(json)
    println(user)  // Salida: User(name=Jane Doe, age=28)
}
```

Tanto Gson como Kotlinx.serialization simplifican trabajar con JSON en aplicaciones Kotlin, elegir uno sobre el otro depende de los requisitos específicos de tu proyecto y preferencias personales.
