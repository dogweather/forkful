---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Fish Shell: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
"Reading command line arguments" signifie simplement que notre programme peut prendre des informations (telles que des mots ou des nombres) directement à partir de la ligne de commande, plutôt que de les avoir écrites dans le code lui-même. Cela peut être utile pour personnaliser l'exécution de notre programme en fonction des entrées de l'utilisateur ou pour traiter de grandes quantités de données sans avoir à les saisir manuellement.

## Comment faire:
Voici un exemple simple avec un programme en Python:
```Fish Shell 

#!/usr/bin/env python
import sys

# Notre programme va prendre l'argument après le nom du fichier
# et le stocker dans une variable appelée "nom"
nom = sys.argv[1]

print("Bonjour, " + nom + " ! Comment vas-tu ?")

// Si l'utilisateur exécute ce programme de la manière suivante:
// python bonjour.py Marie
// Le résultat sera:
// Bonjour, Marie ! Comment vas-tu ?
```

Nous pouvons également utiliser l'argument "-a" pour ajouter un autre argument optionnel et stocker une liste de valeurs dans une variable:
```Fish Shell 

#!/usr/bin/env python
import sys

# Notre programme va prendre deux arguments après le nom du fichier
# et les stocker dans une variable appelée "liste"
liste = sys.argv[2:]

print("La liste des éléments est:",liste)

// Si l'utilisateur exécute ce programme de la manière suivante:
// python liste.py -a pomme banane orange
// Le résultat sera:
// La liste des éléments est: ['pomme', 'banane', 'orange']
```

## Plongée en profondeur:
La lecture des arguments en ligne de commande est une pratique courante en programmation et est utilisée dans de nombreux langages différents, tels que C, Java, et bien sûr Fish Shell. Cette fonctionnalité permet aux développeurs de créer des programmes plus flexibles et interactifs.

Si nous voulons lire des arguments avec des noms spécifiques, nous pouvons utiliser des bibliothèques externes comme "argparse" en Python ou "getopt" en C. Nous pouvons également utiliser des expressions régulières pour filtrer et valider les entrées des utilisateurs avant de les utiliser dans notre programme.

Au niveau de l'implémentation, la lecture des arguments en ligne de commande se fait en accédant à la zone mémoire où ils sont stockés et en utilisant les indices pour récupérer les valeurs souhaitées. La syntaxe peut varier d'un langage à l'autre, mais le concept est le même.

## Voir aussi:
Pour plus d'informations sur la lecture des arguments en ligne de commande, voici quelques sources utiles:
- Documentation Fish Shell: https://fishshell.com/docs/current/cmds/read.html
- Documentation argparse pour Python: https://docs.python.org/3/library/argparse.html
- Tutoriel de traitement des arguments avec getopt en C: https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html