---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Lire les arguments de la ligne de commande en Python, c'est extraire les informations que l'utilisateur a entrées après le nom du script lors de son exécution. Les développeurs font cela pour personnaliser l'exécution du script.

## Comment faire:

Voici comment décoder et utiliser les arguments de la ligne de commande en utilisant le module de Python, `sys`:

```Python
import sys

print("Nom du script:", sys.argv[0])
print("Nombre d'arguments:", len(sys.argv))
print("Les arguments sont:", str(sys.argv))
```
Lorsque vous exécutez le script ci-dessus avec des arguments, l'output pourrait ressembler à cela:

```
$ python3 scriptname.py arg1 arg2 arg3
Nom du script: scriptname.py
Nombre d'arguments: 4
Les arguments sont: ['scriptname.py', 'arg1', 'arg2', 'arg3']
```

## Deep Dive

Historiquement, lire les arguments de la ligne de commande provient de la tradition UNIX où le petit est beau et chaque programme fait une chose mais le fait bien. Cela permet une grande flexibilité et modularité dans les scripts et les programmes.

Alternativement, pour une manipulation plus avancée des arguments de la ligne de commande, le module `argparse` de Python fournit une fonctionnalité sophistiquée et est utilisé pour créer de belles interfaces en ligne de commande.

En termes d'implémentation, `sys.argv` est juste une liste en Python, qui contient les arguments de la ligne de commande passés au script. Avec l'aide de cette liste de chaînes, on peut fournir les arguments nécessaires au programme Python pour permettre une plus grande adaptabilité à l'environnement d'exécution.

## Voir Aussi

Pour une lecture supplémentaire sur ce sujet, visitez les liens suivants:

* L'explication officielle de Python sur `sys` et `sys.argv`: https://docs.python.org/3/library/sys.html
* Un guide sur l'usage de `argparse`: https://docs.python.org/3/howto/argparse.html
* Un aperçu historique de l'utilisation des arguments de la ligne de commande: https://en.wikipedia.org/wiki/Command-line_interface#Arguments