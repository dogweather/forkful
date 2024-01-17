---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Bash: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Lire des arguments de ligne de commande, c'est simplement prendre des valeurs spécifiées par l'utilisateur lors de l'exécution d'un programme en utilisant un terminal. Les programmeurs le font pour rendre leurs programmes plus flexibles et personnalisables pour les utilisateurs.

## Comment faire:

Voici un exemple de code Bash pour lire un argument de ligne de commande en utilisant la variable $1 (la première valeur passée après le nom du script):

```Bash
#!/bin/bash
echo "Voici votre argument: $1"
```

Si l'utilisateur exécute ce script avec l'argument "Bonjour", la sortie sera "Voici votre argument: Bonjour".

## Plongée en profondeur:

La lecture des arguments de ligne de commande a été introduite dans les années 1970 avec l'émergence du système Unix. Alternativement, les programmeurs peuvent également utiliser des options de ligne de commande ou des fichiers de configuration pour permettre aux utilisateurs de personnaliser leurs programmes.

Pour implémenter la lecture des arguments de ligne de commande, les programmeurs utilisent souvent une boucle for pour parcourir toutes les valeurs passées et les stockent dans des variables. Les erreurs de syntaxe ou les valeurs manquantes peuvent être gérées avec des instructions conditionnelles.

## Voir aussi:

- [Tutoriel sur la lecture des arguments de ligne de commande en Bash](https://linuxize.com/post/bash-command-line-arguments/)
- [Livres Linux pour les débutants](https://fr.linux.com/tutorials/bash-a-buyers-guide-for-the-uninitiated/)
- [Documentation officielle sur les arguments de ligne de commande en Bash](https://www.gnu.org/software/bash/manual/html_node/Command_002dLine-Processing.html#Command_002dLine-Processing)