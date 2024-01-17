---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Kotlin: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
La lecture des arguments de la ligne de commande est un moyen pour les programmeurs de spécifier des paramètres au moment de l'exécution d'un programme. Cela permet de rendre les programmes plus flexibles et adaptables.

## Comment faire:
Voici un exemple de code en Kotlin pour lire les arguments de la ligne de commande et les afficher:

```Kotlin
fun main(args: Array<String>) {
    println("Les arguments de la ligne de commande sont: ")
    for (arg in args) {
        println(arg)
    }
}
```

Si nous exécutons ce programme avec les arguments "Hello" et "World", nous obtiendrons la sortie suivante:

```
Les arguments de la ligne de commande sont:
Hello
World
```

## Plongée en profondeur:
Avant l'émergence des interfaces graphiques, la ligne de commande était le principal moyen d'interagir avec un ordinateur. La lecture des arguments de la ligne de commande était donc une tâche courante pour les programmeurs. Aujourd'hui, il existe des alternatives telles que les menus déroulants et les boîtes de dialogue, mais la lecture des arguments de la ligne de commande reste importante pour les programmes qui doivent être exécutés en ligne de commande.

Le moyen le plus courant de lire les arguments de la ligne de commande en Kotlin est d'utiliser la fonction "main" avec un paramètre de type "Array<String>" pour stocker les arguments. Il est également possible d'utiliser la fonction "args" dans la propriété "System" pour accéder aux arguments de la ligne de commande.

## Voir aussi:

- Kotlin Reference: [Command Line Arguments](https://kotlinlang.org/docs/command-line.html)
- Baeldung: [Command Line Argument Parsing with Kotlin](https://www.baeldung.com/kotlin/command-line-arguments)
- Java Program to Parse Command Line Arguments: [GeeksforGeeks](https://www.geeksforgeeks.org/command-line-argument-parsing-in-java/)