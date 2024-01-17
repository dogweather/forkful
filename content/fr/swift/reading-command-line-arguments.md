---
title:                "Lecture des arguments de la ligne de commande"
html_title:           "Swift: Lecture des arguments de la ligne de commande"
simple_title:         "Lecture des arguments de la ligne de commande"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font-ils?

Lire les arguments de ligne de commande est un moyen pour un programme de recevoir des entrées directement depuis la ligne de commande. Les programmeurs le font pour rendre leurs programmes plus interactifs et leur permettre de réagir en temps réel aux entrées de l'utilisateur.

## Comment:

```Swift
let arguments = CommandLine.arguments
print("Les arguments passés dans la ligne de commande sont: \(arguments)")
```

Ce code va prendre tous les arguments passés dans la ligne de commande et les stocker dans une variable "arguments". Ensuite, il les imprime tous à l'écran.

Output:
```
Les arguments passés dans la ligne de commande sont: [nomDuProgramme, premierArgument, deuxièmeArgument]
```

## Plongée en profondeur:

Lire les arguments de ligne de commande est une fonctionnalité présente dans de nombreux langages de programmation depuis les débuts de la programmation informatique. Cela permet aux programmeurs d'interagir avec leurs programmes en leur passant des valeurs directement depuis la ligne de commande plutôt que de les définir à l'avance dans le code.

Il existe également d'autres moyens de recevoir des entrées de l'utilisateur, tels que les fichiers de configuration ou les interfaces graphiques, mais lire les arguments de ligne de commande est souvent privilégié car c'est une méthode simple et rapide.

Dans Swift, lire les arguments de ligne de commande se fait en utilisant la classe CommandLine, qui contient une variable statique "arguments" contenant tous les arguments passés dans la ligne de commande. Il est important de noter que les arguments sont toujours stockés sous forme de chaînes de caractères, il faut donc les convertir en le type souhaité si nécessaire.

## À voir aussi:

- [Documentation officielle de Swift sur CommandLine](https://developer.apple.com/documentation/foundation/commandline