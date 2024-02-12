---
title:                "Työskentely JSON:n kanssa"
aliases: - /fi/kotlin/working-with-json.md
date:                  2024-02-03T19:23:06.492430-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Työskentely JSONin (JavaScript Object Notation) kanssa Kotlinitssa liittyy JSON-datamuodon jäsennys- ja luontitoimiin. Ohjelmoijat tekevät tämän helpottaakseen tietojen vaihtoa sovelluksen eri kerrosten välillä tai kommunikoidakseen web-palveluiden kanssa, JSONin kevyen ja ihmisen luettavan muodon ansiosta.

## Kuinka:
Kotlin ei sisällä valmiiksi tukea JSONille, mutta se hyödyntää kolmansien osapuolien kirjastojen, kuten Googlen `Gson` ja JetBrainsin `Kotlinx.serialization`, tehokkaita ominaisuuksia. Tässä on ohjeet, kuinka voit käyttää molempia työskennelläksesi JSONin kanssa.

### Gsonin käyttö
Lisää Gson-riippuvuus `build.gradle` tiedostoosi:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

JSON-muotoisen merkkijonon jäsennys olioksi ja päinvastoin:
```kotlin
import com.google.gson.Gson

// Määrittele data-luokka
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialisointi
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Tuloste: {"name":"John Doe","age":30}

    // Deserialisointi
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Tuloste: User(name=John Doe, age=30)
}
```

### Kotlinx.serializationin käyttö
Lisää ensin riippuvuus `build.gradle`-tiedostoosi:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Tämän jälkeen sovella `kotlinx-serialization` -lisäosaa koodisi alkuun:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serialisointi ja deserialisointi Kotlinx.serializationin avulla:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Määrittele sarjoitettava data-luokka
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialisointi
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Tuloste: {"name":"Jane Doe","age":28}

    // Deserialisointi
    val user = Json.decodeFromString<User>(json)
    println(user)  // Tuloste: User(name=Jane Doe, age=28)
}
```

Sekä Gson että Kotlinx.serialization yksinkertaistavat JSONin käsittelyä Kotlin-sovelluksissa, ja kumpaa niistä käytetään, riippuu projektisi erityisvaatimuksista ja henkilökohtaisista mieltymyksistäsi.
