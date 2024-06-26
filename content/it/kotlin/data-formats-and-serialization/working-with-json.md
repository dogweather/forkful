---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:16.848624-07:00
description: "Come fare: Kotlin non include supporto integrato per JSON ma si avvale\
  \ delle potenti funzionalit\xE0 di librerie di terze parti come `Gson` di Google\
  \ e\u2026"
lastmod: '2024-03-13T22:44:43.412245-06:00'
model: gpt-4-0125-preview
summary: "Kotlin non include supporto integrato per JSON ma si avvale delle potenti\
  \ funzionalit\xE0 di librerie di terze parti come `Gson` di Google e `Kotlinx.serialization`\
  \ di JetBrains."
title: Lavorare con JSON
weight: 38
---

## Come fare:
Kotlin non include supporto integrato per JSON ma si avvale delle potenti funzionalità di librerie di terze parti come `Gson` di Google e `Kotlinx.serialization` di JetBrains. Ecco come puoi utilizzare entrambi per lavorare con JSON.

### Utilizzando Gson
Aggiungi la dipendenza Gson al tuo file `build.gradle`:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Analizzare una stringa JSON in un oggetto e viceversa:
```kotlin
import com.google.gson.Gson

// Definisci una classe dati
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serializza
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Output: {"name":"John Doe","age":30}

    // Deserializza
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Output: User(name=John Doe, age=30)
}
```

### Utilizzando Kotlinx.serialization
Prima, includi la dipendenza nel tuo `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

In seguito, applica il plugin `kotlinx-serialization` in cima al tuo script di build:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serializzare e deserializzare con Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Definisci una classe dati serializzabile
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serializza
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Output: {"name":"Jane Doe","age":28}

    // Deserializza
    val user = Json.decodeFromString<User>(json)
    println(user)  // Output: User(name=Jane Doe, age=28)
}
```

Sia Gson che Kotlinx.serialization semplificano la lavorazione con JSON in applicazioni Kotlin, la scelta tra l'uno e l'altro dipende dai requisiti specifici del tuo progetto e dalle preferenze personali.
