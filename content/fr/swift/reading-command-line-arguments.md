---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lecture des arguments de la ligne de commande Swift: un guide pratique

## Quoi & Pourquoi?
La lecture des arguments de la ligne de commande signifie que votre programme Swift prend des paramètres depuis le terminal. Il s'agit d'une technique couramment utilisée pour donner des informations à votre programme avant son exécution.

## Comment faire:
Vous pouvez lire les arguments de la ligne de commande à l'aide du tableau `CommandLine.arguments`.

Voici un exemple de son utilisation:

```Swift
let arguments = CommandLine.arguments
print(arguments)
```
Cela affichera tous les arguments passés à votre programme. Par exemple, si vous lancez votre programme avec `./myProgram argument1 argument2`, la sortie sera `["./myProgram", "argument1", "argument2"]`.

## Plongeons plus loin
La possibilité de lire les arguments de la ligne de commande existe depuis les débuts de la programmation. C'est une façon essentielle de paramétrer les applications en dehors du code lui-même. En Swift, les arguments de ligne de commande sont stockés dans un tableau de chaînes. Le premier élément est toujours le nom du programme lui-même.

Il existe des alternatives, comme utiliser une bibliothèque de parsing d'arguments de ligne de commande, qui peut faciliter l'utilisation de commandes plus complexes. Cependant, pour les cas simples, l'utilisation de `CommandLine.arguments` est suffisante.

## Voir aussi
Utilisation efficace des arguments de ligne de commande en Swift: https://www.hackingwithswift.com/read/12/overview
Guide de Apple sur les outils de ligne de commande: https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CommandLine.html