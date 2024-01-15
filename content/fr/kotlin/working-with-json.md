---
title:                "Travailler avec json"
html_title:           "Kotlin: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur, il est très probable que vous ayez rencontré le format JSON. Que ce soit pour échanger des données avec un serveur ou pour stocker des informations dans une base de données, JSON est un format de données très populaire dans l'univers du développement. Dans cet article, nous allons vous montrer pourquoi il est utile de travailler avec JSON en utilisant le langage de programmation Kotlin.

# Comment faire

Pour travailler avec JSON en Kotlin, il existe plusieurs librairies disponibles qui facilitent la manipulation de ce format de données. L'une des plus populaires est kotlinx.serialization, développée par JetBrains. Voici un exemple d'utilisation pour transformer un objet Kotlin en JSON :

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

val user = User("John Doe", 25)
val json = Json.encodeToString(user)
println(json) // Output: {"name":"John Doe","age":25}
```

Et pour transformer un JSON en un objet Kotlin :

```Kotlin
val jsonString = """{"name":"Jane Doe","age":30}"""
val user = Json.decodeFromString<User>(jsonString)
println(user) // Output: User(name=Jane Doe, age=30)
```

Comme vous pouvez le constater, kotlinx.serialization utilise des annotations pour identifier les propriétés de l'objet à sérialiser ou désérialiser. Il est également possible de personnaliser le format de sortie en utilisant des annotations supplémentaires.

# Deep Dive

Si vous souhaitez en apprendre davantage sur la manipulation de JSON en Kotlin, il est important de comprendre sa structure. JSON est composé de paires clé-valeur, où la clé est une chaîne de caractères et la valeur peut être de différents types tels que des chaînes, des nombres ou même d'autres objets JSON. Il existe également des tableaux JSON, similaires à des listes, qui peuvent contenir plusieurs valeurs dans un ordre défini.

Pour accéder aux données d'un objet JSON en Kotlin, il est possible d'utiliser des opérateurs d'accès tels que ".get" pour les paires clé-valeur ou "[index]" pour les tableaux. Vous pouvez également utiliser des expressions régulières pour rechercher des données spécifiques dans un JSON plus complexe.

Enfin, il est important de noter que kotlinx.serialization n'est pas la seule librairie disponible pour travailler avec JSON en Kotlin. Vous pouvez également utiliser des librairies telles que Gson ou Jackson, selon vos besoins et préférences.

# Voir aussi

- [kotlinx.serialization documentation](https://github.com/Kotlin/kotlinx.serialization)
- [Comparaison des librairies de manipulation de JSON en Kotlin](https://proandroiddev.com/which-json-library-to-use-in-kotlin-9370e6f72e26)
- [Guide complet sur la syntaxe JSON](https://www.json.org/json-en.html)