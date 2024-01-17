---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Elm: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Lire les arguments de ligne de commande est une compétence essentielle pour tout programmeur Elm. Cela implique de fournir des entrées spécifiques lors de l'exécution d'un programme à partir de la ligne de commande, ce qui permet de personnaliser son comportement. Les programmeurs le font pour rendre leurs programmes plus flexibles et personnalisables pour les utilisateurs.

## Comment Faire:
Dans Elm, il existe une fonction appelée `getArgs` qui permet de lire les arguments de ligne de commande. Voici un exemple de code qui utilise cette fonction:

```
Elm Platform-Cli kernel version 0.19.1
----------------------------
source-repository-package : Elm.GetArgs
source-distribution : This is not a library!
source-homepage : N/A
source-author : N/A
source-synopsis : Using getArgs function to read command line arguments in Elm.
```

## Deep Dive:
La lecture des arguments de ligne de commande est une pratique courante en informatique depuis les débuts de la programmation. Auparavant, les programmes étaient souvent exécutés à partir d'un terminal, et les lignes de commande étaient la seule façon de spécifier des entrées.

Il existe d'autres façons de passer des entrées aux programmes, telles que les interfaces graphiques ou les fichiers de configuration. Cependant, la lecture des arguments de ligne de commande reste une méthode simple et efficace pour les programmeurs Elm.

L'implémentation de la fonction `getArgs` est basée sur la bibliothèque standard de Haskell, qui est utilisée par Elm pour gérer les entrées et sorties.

## Voir Aussi:
Pour en savoir plus sur la lecture des arguments de ligne de commande en Elm, vous pouvez consulter la documentation officielle de la fonction `getArgs`, ainsi que des exemples de code sur des sites comme GitHub et Stack Overflow. Vous pouvez également explorer d'autres moyens d'interagir avec les utilisateurs de vos programmes en utilisant des interfaces graphiques ou d'autres méthodes de communication.