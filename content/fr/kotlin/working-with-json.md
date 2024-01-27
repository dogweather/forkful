---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
JSON (JavaScript Object Notation) est un format léger d'échange de données facile à lire pour les humains et à analyser pour les machines. Les programmeurs l'utilisent pour échanger des données entre serveur et client, configurer des applications, et stocker des informations simples.

## How to:
Voici comment gérer du JSON en Kotlin :

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// On définit un data class pour représenter nos données 
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Création d'une instance User
    val user = User("Jean", 30)

    // Sérialisation : Data Class -> JSON String
    val jsonString = Json.encodeToString(user)
    println(jsonString) // {"name":"Jean","age":30}

    // Désérialisation : JSON String -> Data Class
    val userObj = Json.decodeFromString<User>(jsonString)
    println(userObj) // User(name=Jean, age=30)
}
```
Ici on utilise la librairie `kotlinx.serialization`. Pour sérialiser, `encodeToString()`. Pour désérialiser, `decodeFromString()`.

## Deep Dive
JSON existe depuis les débuts des années 2000. Alternatives incluent XML et YAML, mais JSON est favorisé pour sa simplicité. Kotlin utilise `kotlinx.serialization` comme bibliothèque standard pour la manipulation de JSON, offrant de bonnes performances grâce à la sérialisation en-ligne du code.

## See Also
- Documentation officielle de Kotlin Serialization: [kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- Spécification JSON: [json.org](https://www.json.org/json-fr.html)
- Comparaison de JSON et XML: [w3schools.com](https://www.w3schools.com/js/js_json_xml.asp)
