---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:33.279339-07:00
description: "Hoe: Om met JSON in Kotlin te werken, kun je de `kotlinx.serialization`\
  \ bibliotheek gebruiken. Hier is een eenvoudig voorbeeld van het serialiseren en\u2026"
lastmod: '2024-03-13T22:44:50.789172-06:00'
model: gpt-4-0125-preview
summary: Om met JSON in Kotlin te werken, kun je de `kotlinx.serialization` bibliotheek
  gebruiken.
title: Werken met JSON
weight: 38
---

## Hoe:
Om met JSON in Kotlin te werken, kun je de `kotlinx.serialization` bibliotheek gebruiken. Hier is een eenvoudig voorbeeld van het serialiseren en deserialiseren van een dataklasse.

```Kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.encodeToString
import kotlinx.serialization.decodeFromString

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json { prettyPrint = true }
    val userData = User("John Doe", 30)
    
    // Serialiseren naar JSON
    val jsonString = json.encodeToString(userData)
    println(jsonString)
    
    // Deserialiseren vanuit JSON
    val userObj = json.decodeFromString<User>(jsonString)
    println(userObj)
}
```

Voorbeelduitvoer:

```
{
    "name": "John Doe",
    "age": 30
}
User(name=John Doe, age=30)
```

## Dieper duiken
De eenvoudige syntaxis van JSON vindt zijn oorsprong in JavaScript, maar het is nu taalonafhankelijk. Alternatieven zoals XML zijn meer uitgebreid. Wanneer je met JSON werkt in Kotlin, neemt de `kotlinx.serialization` bibliotheek het zware werk uit handen, door automatisch Kotlin-objecten naar en van JSON te converteren met annotaties. Het ondersteunt complexe gegevenstypes en pakt randgevallen aan, maar handmatig ontleden van JSON is ook een optie als je strakkere controle nodig hebt.

## Zie ook
- Kotlin Serialisatiegids: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- Inleiding tot JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
