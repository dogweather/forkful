---
title:                "Kotlin: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec JSON?

JSON (JavaScript Object Notation) est un format de données largement utilisé pour l'échange de données entre des applications. Il est léger, facile à lire et à comprendre, et est également pris en charge par la plupart des langages de programmation, y compris Kotlin. Travailler avec JSON peut vous permettre d'envoyer et de recevoir des données efficacement, ce qui est essentiel pour la plupart des applications modernes.

## Comment faire

Il existe deux façons principales de travailler avec JSON en Kotlin: en utilisant une bibliothèque externe ou en utilisant les classes intégrées de Kotlin pour faciliter le traitement de JSON.

### Utiliser une bibliothèque externe

Une façon populaire de travailler avec JSON en Kotlin est d'utiliser une bibliothèque externe telle que Gson ou Jackson. Ces bibliothèques offrent des fonctions pratiques pour convertir des objets Kotlin en JSON et vice versa. Voici un exemple de code utilisant la bibliothèque Gson:

```Kotlin
// Créer un objet Kotlin à convertir en JSON
data class Person(val name: String, val age: Int)

// Importer la bibliothèque Gson
import com.google.gson.Gson

// Convertir l'objet en JSON
val person = Person("Jeanne", 25)
val json = Gson().toJson(person)

// Afficher le résultat
println(json) // {"name":"Jeanne", "age":25}
```

### Utiliser les classes intégrées Kotlin

Kotlin dispose également de classes intégrées pour travailler avec JSON, telles que JSONObject et JSONArray. Vous pouvez les utiliser pour créer et manipuler des objets JSON sans avoir besoin d'une bibliothèque externe. Jetons un coup d'oeil à un exemple:

```Kotlin
// Importer les classes JSONObject et JSONArray
import org.json.JSONObject
import org.json.JSONArray

// Créer un objet JSON avec valeurs initiales
val person = JSONObject("""{"name":"Pierre", "age":30}""")

// Ajouter une propriété à l'objet
person.put("city", "Paris")

// Créer un tableau JSON avec plusieurs objets
val people = JSONArray()
people.put(person)
people.put(JSONObject("""{"name":"Sophie", "age":28}"""))

// Afficher le résultat
println(people) // [{"name":"Pierre", "age":30, "city":"Paris"}, {"name":"Sophie", "age":28}]
```

## Plongée profonde

Travailler avec JSON en Kotlin peut également inclure la gestion des erreurs et la manipulation de données plus complexes, telles que les tableaux et les objets imbriqués. Si vous cherchez à approfondir vos connaissances sur la manipulation de JSON en Kotlin, vous pouvez consulter la documentation officielle de Kotlin ainsi que des tutoriels en ligne.

## Voir aussi

- Documentation officielle de Kotlin pour la manipulation de JSON: https://kotlinlang.org/docs/reference/type-safe-builders.html#json
- Tutoriel sur la manipulation de JSON en Kotlin: https://www.raywenderlich.com/730-moshi-tutorial-for-android-getting-started
- Bibliothèque Gson: https://github.com/google/gson
- Bibliothèque Jackson: https://github.com/FasterXML/jackson