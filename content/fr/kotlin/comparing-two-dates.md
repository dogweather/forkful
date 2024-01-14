---
title:    "Kotlin: Comparaison de deux dates"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi 

Comparer deux dates est une tâche courante dans la programmation, en particulier dans les applications de gestion de calendrier ou de réservation. Cela permet de vérifier si une date est antérieure, ultérieure ou égale à une autre, et peut être utile dans de nombreuses fonctions.

## Comment Faire

Pour comparer deux dates en Kotlin, vous pouvez utiliser la méthode `compareTo()` de la classe `LocalDate`. Voici un exemple de code pour comparer deux dates et afficher le résultat :

````Kotlin
// Déclaration et initialisation des dates
val date1 = LocalDate.of(2020, 5, 10)
val date2 = LocalDate.of(2020, 5, 15)

// Comparaison des dates
val comparaison = date1.compareTo(date2)

// Affichage du résultat
println(comparaison) // Cela affichera -5 car la date 1 est antérieure à la date 2
````

Vous pouvez également utiliser les opérateurs `<` (inférieur à), `>` (supérieur à) et `==` (égal à) pour comparer les dates, comme dans l'exemple ci-dessous :

````Kotlin
// Déclaration et initialisation des dates
val date1 = LocalDate.of(2020, 5, 10)
val date2 = LocalDate.of(2020, 5, 15)

// Comparaison avec l'opérateur < (inférieur à)
if(date1 < date2) {
    println("La date 1 est antérieure à la date 2")
}

// Comparaison avec l'opérateur == (égal à)
if(date1 == date2) {
    println("Les deux dates sont égales")
}
````

## Plongée en Profondeur 

Il existe une différence entre la méthode `compareTo()` et les opérateurs de comparaison pour les dates en Kotlin. La méthode `compareTo()` utilise le concept de comparaison d'ordre ou de tri, tandis que les opérateurs de comparaison comparent les dates de manière absolue.

De plus, il est important de noter que les dates sont comparées en fonction de leur ordre chronologique et non pas de leur format ou de leur chaîne de caractères correspondante. Par exemple, la date "10/05/2020" sera considérée comme égale à "2020-05-10" lors de la comparaison.

## Voir Aussi 

Pour en savoir plus sur les dates en Kotlin, vous pouvez consulter les ressources suivantes :

- Documentation officielle de Kotlin sur les dates : https://kotlinlang.org/docs/reference/datetime.html
- Tutoriel sur la manipulation des dates en Kotlin : https://www.geeksforgeeks.org/kotlin-date-manipulation/
- Exemples pratiques pour gérer les dates en Kotlin : https://dev.to/kotlin/examples-of-kotlin-datetime-manipulations-4okc