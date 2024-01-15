---
title:                "Lecture des arguments de la ligne de commande"
html_title:           "Kotlin: Lecture des arguments de la ligne de commande"
simple_title:         "Lecture des arguments de la ligne de commande"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté, vous savez probablement déjà que les arguments de ligne de commande sont un élément essentiel de la programmation. Ils permettent aux utilisateurs de votre programme de passer des valeurs spécifiques au moment de l'exécution, ce qui peut être très utile pour personnaliser l'expérience utilisateur ou pour automatiser certaines tâches. Dans cet article, nous allons explorer comment lire les arguments de ligne de commande en utilisant Kotlin.

## Comment faire

Pour lire les arguments de ligne de commande en Kotlin, il existe une classe prédéfinie appelée "args" qui stocke tous les arguments passés lors de l'exécution du programme. Pour accéder à ces arguments, il suffit d'itérer sur cette liste en utilisant une boucle "foreach" et d'afficher chaque argument. Voici un exemple de code :

```Kotlin
fun main(args: Array<String>) {
    args.forEach { arg ->
        println(arg)
    }
}
```

Si nous exécutons ce programme avec les arguments "Bonjour" et "Monde", nous obtenons la sortie suivante :

```
Bonjour
Monde
```

Nous pouvons également utiliser ces arguments pour effectuer des calculs ou des opérations spécifiques, en utilisant des fonctions prédéfinies telles que "toInt()" ou "toDouble()" pour convertir les valeurs en nombres. Voici un autre exemple qui additionne deux nombres passés en arguments :

```Kotlin
fun main(args: Array<String>) {
    val nb1 = args[0].toInt()
    val nb2 = args[1].toInt()
    println("La somme de $nb1 et $nb2 est égale à ${nb1 + nb2}")
}
```

Si nous exécutons ce programme avec les arguments "10" et "5", nous obtenons la sortie suivante :

```
La somme de 10 et 5 est égale à 15
```

## Plongée en profondeur

Maintenant que vous savez comment lire les arguments de ligne de commande en Kotlin, il est bon de comprendre comment ils sont organisés. En utilisant la classe "args", vous pouvez également accéder aux informations telles que le nombre total d'arguments ("args.size") ou l'argument à une position particulière ("args[index]"). De plus, il est possible de les combiner avec des structures de contrôle telles que "if" ou "when" pour gérer différents scénarios.

Par exemple, si nous voulons afficher un message d'erreur si aucun argument n'est passé, nous pouvons le faire avec le code suivant :

```Kotlin
fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println("Veuillez fournir au moins un argument.")
    } else {
        args.forEach { arg ->
            println(arg)
        }
    }
}
```

## Voir aussi

Vous pouvez également consulter d'autres articles sur Kotlin et la programmation en général :
- [Documentation officielle de Kotlin](https://kotlinlang.org/docs/reference/)
- [Comment utiliser des boucles en Kotlin](https://dev.to/banej/kotlin-for-loop-explained-1dkg)
- [Introduction à la programmation en Kotlin](https://www.codecademy.com/learn/learn-kotlin)

Merci d'avoir lu cet article sur la lecture des arguments de ligne de commande en Kotlin ! Nous espérons que cela vous a été utile dans votre parcours de développement.