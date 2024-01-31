---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Les expressions régulières, ou regex, filtrent et manipulent des chaînes de caractères. Les devs les utilisent pour valider des données comme des emails, chercher ou remplacer du texte.

## How to:
Un exemple simple pour chercher un mot dans une chaîne de caractères :
```Kotlin
fun main() {
    val regex = Regex("Kotlin")
    val text = "J'adore programmer en Kotlin !"
    println(regex.containsMatchIn(text)) // Affiche : true
}
```

Pour remplacer un mot par un autre :
```Kotlin
fun main() {
    val regex = Regex("Java")
    val text = "Java c'est bien, mais Kotlin c'est mieux."
    val replacedText = text.replace(regex, "Kotlin")
    println(replacedText) // Affiche : Kotlin c'est bien, mais Kotlin c'est mieux.
}
```

Extraire des données avec des groupes :
```Kotlin
fun main() {
    val regex = Regex("user_(\\d+)")
    val text = "Les identifiants sont user_1234, user_5678."
    val ids = regex.findAll(text).map { it.groupValues[1] }.joinToString(", ")
    println(ids) // Affiche : 1234, 5678
}
```

## Deep Dive
Les regex existent depuis les années 1950, développées d'abord pour la théorie des automates. Les alternatives incluent l'utilisation de parseurs pour des tâches complexes ou des bibliothèques spécialisées comme `apache-commons` en Java. Kotlin les implémente via `java.util.regex`, donc leur comportement est identique à Java.

## See Also
- Kotlin Regex documentation: [kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Online Regex Tester: [regex101.com](https://regex101.com)
- Regex usage in Java for historical context: [docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
