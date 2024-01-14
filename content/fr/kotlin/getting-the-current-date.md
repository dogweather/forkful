---
title:    "Kotlin: Obtenir la date actuelle"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Obtenir la date actuelle est une tâche courante dans de nombreuses applications. Il est important pour de nombreuses raisons telles que les horaires, les rappels, et la gestion des tâches. Heureusement, Kotlin offre des moyens simples et efficaces pour accéder à la date actuelle.

# Comment faire

Voici comment vous pouvez obtenir facilement la date actuelle en utilisant Kotlin :

```
fun main() {
    // Obtention de la date actuelle
    val currentDate = Date()

    // Formattage de la date avec un motif spécifique
    val formattedDate = SimpleDateFormat("dd.MM.yyyy").format(currentDate)

    // Affichage de la date
    println("La date actuelle est : $formattedDate")
}
```

Output : La date actuelle est : 22.05.2021

# Plongée en profondeur

Maintenant que vous savez comment obtenir la date actuelle, voici quelques informations supplémentaires sur la façon dont cela fonctionne en interne :

- La première ligne de code utilise la classe `Date` pour créer une instance de la date actuelle. Cette classe est fournie par Java et Kotlin la prend en charge également.
- La deuxième ligne utilise la classe `SimpleDateFormat` pour formater la date en utilisant un motif spécifique. Il existe de nombreux autres motifs pour formater la date selon vos besoins.
- Enfin, la troisième ligne affiche simplement la date formatée en utilisant une chaîne de formatage.

# Voir aussi

- [Documentation Kotlin sur les Dates](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Documentation Java sur les Dates](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Guide des motifs de date et d'heure](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)