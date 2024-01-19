---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Lire des arguments ligne de commande en Kotlin, c'est déchiffrer les instructions qu'un utilisateur a donné à votre programme via l'interface de ligne de commande. On fait ça pour permettre aux utilisateurs d'interagir avec nos applications de façon flexible.

## Comment faire :
Pour lire les arguments ligne de commande en Kotlin, on utilise `args: Array<String>`. Voici un exemple simple.

```Kotlin 
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

Quand vous exécutez ce programme avec des arguments comme 'Salut' et 'Bonjour', voici la sortie :

``` 
Salut
Bonjour
```

## Approfondissement
Historiquement, la lecture des arguments de ligne de commande est une tradition des premiers jours de l'informatique, lorsque l'interface graphique n'était pas encore une chose.

En Kotlin, vous pouvez aussi utiliser `readLine()` pour lire les entrées de l'utilisateur durant l'exécution du programme. Mais ce n'est pas la même chose que la lecture des arguments de ligne de commande qui sont donnés en une fois avant que le programme ne commence à s'exécuter.

En coulisses, Kotlin fait en sorte que les arguments passés à votre programme soient disponibles via l'objet `args` de type `Array<String>`. Vous pouvez parcourir cet array comme tout autre tableau en Kotlin.

## Voir aussi :
Pour plus d'informations sur la lecture des arguments de ligne de commande  et de l'entrée utilisateur, consultez ces liens :

1. Les fonctions main en Kotlin: [https://kotlinlang.org/docs/functions.html#main-function]
2. Tutoriel de Kotlin pour les débutants: Arguments de Ligne de Commande: [https://www.programiz.com/kotlin-programming/command-line-argument]