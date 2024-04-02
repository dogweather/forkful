---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:03.945968-07:00
description: "Trabalhar com JSON (JavaScript Object Notation) em Kotlin envolve a\
  \ an\xE1lise e gera\xE7\xE3o de dados JSON. Programadores fazem isso para trocar\
  \ dados facilmente\u2026"
lastmod: '2024-03-13T22:44:46.563445-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com JSON (JavaScript Object Notation) em Kotlin envolve a an\xE1\
  lise e gera\xE7\xE3o de dados JSON. Programadores fazem isso para trocar dados facilmente\u2026"
title: Trabalhando com JSON
weight: 38
---

## O Que & Por Quê?
Trabalhar com JSON (JavaScript Object Notation) em Kotlin envolve a análise e geração de dados JSON. Programadores fazem isso para trocar dados facilmente entre diferentes camadas de uma aplicação ou se comunicar com serviços web, devido ao formato leve e legível por humanos do JSON.

## Como fazer:
Kotlin não inclui suporte embutido para JSON, mas aproveita os recursos poderosos de bibliotecas de terceiros como `Gson`, do Google, e `Kotlinx.serialization`, da JetBrains. Aqui está como você pode usar ambos para trabalhar com JSON.

### Usando Gson
Adicione a dependência do Gson ao seu arquivo `build.gradle`:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

Analisando uma string JSON para um objeto e vice-versa:
```kotlin
import com.google.gson.Gson

// Defina uma classe de dados
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serializar
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // Saída: {"name":"John Doe","age":30}

    // Desserializar
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // Saída: User(name=John Doe, age=30)
}
```

### Usando Kotlinx.serialization
Primeiro, inclua a dependência no seu `build.gradle`:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

Depois, aplique o plugin `kotlinx-serialization` no topo do seu script de construção:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Serializando e desserializando com Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Defina uma classe de dados serializável
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serializar
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // Saída: {"name":"Jane Doe","age":28}

    // Desserializar
    val user = Json.decodeFromString<User>(json)
    println(user)  // Saída: User(name=Jane Doe, age=28)
}
```

Tanto Gson quanto Kotlinx.serialization simplificam o trabalho com JSON em aplicações Kotlin, escolher um ou outro depende dos requisitos específicos do seu projeto e preferências pessoais.
