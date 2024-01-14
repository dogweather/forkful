---
title:    "Kotlin: La lecture des arguments en ligne de commande"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation des arguments de ligne de commande est un moyen efficace et rapide de fournir des entrées aux programmes Kotlin. Cela peut être utile dans de nombreux cas, comme dans les programmes en ligne de commande, les scripts shell ou encore les applications en mode texte.

## Comment Faire

Pour lire les arguments de ligne de commande en Kotlin, nous pouvons utiliser la fonction "main" avec un paramètre de type "Array<String>". Ensuite, nous pouvons utiliser la propriété "args" pour accéder aux arguments passés lors de l'exécution du programme.

```Kotlin
fun main(args: Array<String>) {
    // Accéder au premier argument
    println("Le premier argument est: ${args[0]}") 

    // Accéder à tous les arguments en utilisant une boucle
    for (arg in args) {
        println("Argument: $arg")
    }
}
```
Exemple d'exécution du programme avec les arguments "Hello" et "World":
```Kotlin
$ kotlin MainKt Hello World
> Le premier argument est: Hello 
> Argument: Hello
> Argument: World
```

## Plongée Profonde

Nous pouvons également spécifier des options pour nos arguments de ligne de commande en utilisant les préfixes "-" ou "--". Par exemple, si nous voulons spécifier une option pour activer le mode débogage, nous pouvons utiliser "--debug" comme argument.

De plus, nous pouvons utiliser la bibliothèque Kotlin-Argparser pour une gestion plus avancée des arguments de ligne de commande. Cette bibliothèque permet de définir des options et des valeurs pour plus de flexibilité dans nos programmes.

## Voir Aussi

- [Documentation sur les arguments de ligne de commande en Kotlin](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Bibliothèque Kotlin-Argparser](https://github.com/xenomachina/kotlin-argparser)