---
title:                "Réusinage"
aliases:
- /fr/kotlin/refactoring/
date:                  2024-01-26T01:43:01.513441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Réusinage"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Le refactoring est le processus de modification du code existant pour améliorer sa structure, sa lisibilité et sa performance sans changer son comportement externe. Les programmeurs refactorisent pour rendre le code plus facile à maintenir, pour simplifier l'ajout de nouvelles fonctionnalités, et pour détecter et corriger plus facilement les bugs.

## Comment faire :
Voici un extrait de code Kotlin montrant un défaut de code commun et sa version refactorisée. Nous commençons avec un bloc de code qui en fait trop :

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("ID de commande : ${order.id}")
        // Calcul du total de la commande
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Appliquer la remise
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total : $total")
        // Plus de traitement...
    }
}
```

Refactorisé pour une meilleure lisibilité et séparation des préoccupations :

```kotlin
fun printOrderSummary(order: Order) {
    print("ID de commande : ${order.id}")
    val total = calculateTotal(order)
    print("Total : $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Pas d'exemple de sortie ici puisque nous n'avons pas changé la fonctionnalité, mais la lisibilité et la maintenabilité du code ont été considérablement améliorées !

## Plongée Profonde
Le refactoring, en tant que concept, existe depuis le début de la programmation, mais il a vraiment décollé en tant que discipline dans les années 1990, surtout après que Martin Fowler a publié "Refactoring: Improving the Design of Existing Code" en 1999. Ce livre a donné un nom à la pratique et a défini une méthode organisée pour l'appliquer, incluant un catalogue de techniques de refactoring.

En comparant le refactoring aux alternatives : vous pourriez réécrire le code à partir de zéro (risqué et chronophage), ou simplement faire des ajouts (conduit au bloatware et à une dette technique potentielle). Le refactoring trouve le juste milieu - il modernise et nettoie tout en gardant les risques faibles.

En termes de mise en œuvre, il est essentiel de disposer d'un ensemble robuste de tests avant de commencer à refactoriser pour s'assurer de ne pas changer accidentellement le comportement du programme. De nombreux IDE modernes (y compris IntelliJ pour Kotlin) disposent d'outils de refactoring automatiques pour renommer des variables, extraire des méthodes, et plus encore, ce qui peut accélérer le processus et réduire les erreurs.

## Voir Aussi
- "Refactoring: Improving the Design of Existing Code" de Martin Fowler (pour le travail fondateur sur ce sujet)
- Documentation Kotlin sur les conventions de codage : [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (pour comprendre la manière 'Kotlin' d'écrire du code propre)
- Support JetBrains pour le refactoring dans IntelliJ IDEA : [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (pour une utilisation pratique des outils de refactoring)
- Guide de Google sur le refactoring à grande échelle : [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (pour des aperçus sur la façon de relever des défis de refactoring plus importants)
