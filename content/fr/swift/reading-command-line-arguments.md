---
title:                "Swift: Lecture des arguments de ligne de commande"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Swift, vous avez probablement déjà entendu parler de la lecture des arguments de ligne de commande. Mais pourquoi est-ce important et pourquoi devriez-vous vous en soucier ? Eh bien, la lecture des arguments de ligne de commande est essentielle pour créer des applications interactives et flexibles. En utilisant les arguments de ligne de commande, vous pouvez modifier le comportement de votre application à chaque exécution sans avoir à modifier votre code source. Dans cet article, nous allons plonger plus en profondeur dans la lecture des arguments de ligne de commande en Swift.

## Comment faire

Pour commencer, vous devez comprendre les deux types d'arguments de ligne de commande : les options et les arguments. Les options sont des indicateurs qui modifient le comportement de votre application, tandis que les arguments sont des valeurs spécifiques que vous souhaitez modifier. Alors, comment pouvons-nous lire ces arguments en Swift ?

Tout d'abord, vous devez importer la bibliothèque Foundation, qui contient les outils nécessaires pour lire les arguments de ligne de commande. Ensuite, vous pouvez utiliser la fonction `CommandLine.arguments` pour accéder à la liste complète des arguments passés lors de l'exécution de votre application. Par exemple :

```Swift
import Foundation

let arguments = CommandLine.arguments
print(arguments)
```

Dans cet exemple, nous importons la bibliothèque Foundation, puis utilisons la variable `arguments` pour stocker la liste des arguments de ligne de commande. Ensuite, nous utilisons simplement la fonction `print()` pour afficher cette liste à la console. Si vous exécutez cette application en utilisant l'option `-a` et les arguments `10` et `Hello`, la sortie sera la suivante :

```Swift
["-a", "10", "Hello"]
```

Maintenant que vous savez comment accéder à la liste des arguments, vous pouvez utiliser des boucles et des conditions pour effectuer différentes actions en fonction des arguments passés. Par exemple, vous pouvez utiliser une boucle `for` pour parcourir tous les arguments et utiliser une structure `switch` pour vérifier si un argument est une option ou un argument. Voici un exemple :

```Swift
import Foundation

let arguments = CommandLine.arguments

for (index, argument) in arguments.enumerated() {
    switch argument {
    case "-a":
        print("[Option] The value passed after this option is \(arguments[index + 1])")
    default:
        print("[Argument] The argument at index \(index) is \(argument)")
    }
}
```

Dans cet exemple, nous utilisons `enumerated()` pour obtenir à la fois l'indice et la valeur de chaque argument dans la boucle `for`. Ensuite, dans la structure `switch`, nous vérifions si l'argument est égal à notre option `-a` et si c'est le cas, nous utilisons l'indice pour accéder à la valeur suivante dans la liste qui se trouve être notre argument. Si vous exécutez cette application avec les mêmes arguments que précédemment, la sortie sera :

```
[Option] The value passed after this option is 10
[Argument] The argument at index 2 is Hello
```

Comme vous pouvez le voir, nous avons pu accéder à la valeur `10` en utilisant l'indice et en ajoutant 1 à celui-ci.

## Plongée en profondeur

Maintenant que vous avez une compréhension de base de la lecture des arguments de ligne de commande en Swift, il est important de noter qu'il existe d'autres méthodes que vous pouvez utiliser pour les manipuler plus avancées. Par exemple, vous pouvez utiliser la bibliothèque ArgumentParser pour créer une interface plus propre et plus conviviale pour votre application en utilisant un schéma de ligne de commande. Vous pouvez également créer des tests unitaires pour tester comment votre application réagit à différents arguments et options.

## Voir aussi

Voici quelques liens utiles pour en apprendre plus sur la lecture des arguments de ligne de commande en Swift :

- [Document officiel Apple sur la lecture des arguments de ligne de commande en Swift](https://developer.apple.com/documentation/swift/commandline/arguments)
- [Tutoriel sur la lecture des arguments de ligne de commande en Swift](https://www.learnswiftwithbob.com/blog/reading-command-line-arguments-in-swift)
- [Bibliothèque ArgumentParser pour créer une interface de ligne de commande en Swift](https://github.com/apple/swift-argument-parser)