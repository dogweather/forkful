---
title:                "Travailler avec JSON"
aliases:
- fr/kotlin/working-with-json.md
date:                  2024-02-03T19:23:02.097915-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec JSON (JavaScript Object Notation) en Kotlin implique l'analyse et la génération de données JSON. Les programmeurs font cela pour échanger facilement des données entre différentes couches dans une application, ou communiquer avec des services web, en raison du format léger et lisible de JSON.

## Comment faire :
Kotlin n'inclut pas de support intégré pour JSON mais tire parti des fonctionnalités puissantes de bibliothèques tierces telles que `Gson` de Google et `Kotlinx.serialization` de JetBrains. Voici comment vous pouvez utiliser les deux pour travailler avec JSON.

### Utiliser Gson
Ajoutez la dépendance Gson à votre fichier `build.gradle` :
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Analyser une chaîne JSON en un objet et vice-versa :
```kotlin
import com.google.gson.Gson

// Définir une classe de données
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Sérialiser
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Sortie : {"name":"John Doe","age":30}

    // Désérialiser
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Sortie : User(name=John Doe, age=30)
}
```

### Utiliser Kotlinx.serialization
D'abord, incluez la dépendance dans votre `build.gradle` :
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Ensuite, appliquez le plugin `kotlinx-serialization` en haut de votre script de build :
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Sérialiser et désérialiser avec Kotlinx.serialization :
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Définir une classe de données sérialisable
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Sérialiser
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Sortie : {"name":"Jane Doe","age":28}

    // Désérialiser
    val user = Json.decodeFromString<User>(json)
    println(user)  // Sortie : User(name=Jane Doe, age=28)
}
```

Gson et Kotlinx.serialization simplifient le travail avec JSON dans les applications Kotlin, choisir l'un ou l'autre dépend de vos besoins spécifiques de projet et de vos préférences personnelles.
