---
title:                "Comprendre les arguments en ligne de commande"
html_title:           "Python: Comprendre les arguments en ligne de commande"
simple_title:         "Comprendre les arguments en ligne de commande"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Le passage d'arguments en ligne de commande peut sembler intimidant pour les débutants en programmation, mais c'est en fait une compétence très utile à maîtriser. Cela vous permet de créer des programmes plus flexibles et faciles à utiliser en donnant à vos utilisateurs la possibilité de personnaliser l'exécution de votre code.

## Comment faire

Pour lire les arguments en ligne de commande en Python, vous pouvez utiliser le module `sys` et la liste `argv`. Voici un exemple de code:

```Python
import sys

# stocke les arguments dans une liste
arguments = sys.argv 

# le premier élément de la liste est le nom du fichier
# les arguments personnalisés commencent à l'index 1
# vous pouvez accéder à chaque argument en utilisant son index
print("Mon premier argument est:", arguments[1])

# vous pouvez également vérifier le nombre d'arguments passés
print("J'ai reçu", len(arguments)-1, "arguments en tout.")
```

Essayez de l'exécuter avec différents arguments et voyez comment cela affecte la sortie.

```
$ python arguments.py arg1 arg2
Mon premier argument est: arg1
J'ai reçu 2 arguments en tout.
```

## Plongée en profondeur

En plus de lire les arguments passés en ligne de commande, vous pouvez également utiliser des options pour rendre votre code encore plus souple. Le module `argparse` vous permet de définir des options avec des arguments personnalisés et de les utiliser dans votre programme. Voici un exemple:

```Python
import argparse

# crée un objet ArgumentParser pour gérer les options
parser = argparse.ArgumentParser(description='Un programme qui double un nombre.')

# ajoute une option "-n" qui prend un argument de type entier
parser.add_argument('-n', type=int, help='Le nombre à doubler')

# parse les arguments passés en ligne de commande
args = parser.parse_args()

# multiplie l'argument avec lui-même et affiche le résultat
print("Le double de", args.n, "est", args.n * 2)
```

Essayez de l'exécuter avec l'option `-n` et un nombre de votre choix.

```
$ python options.py -n 5
Le double de 5 est 10
```

## Voir aussi

- [La documentation officielle de Python sur les arguments de ligne de commande](https://docs.python.org/fr/3.8/library/sys.html#sys.argv)
- [La documentation officielle de Python sur le module argparse](https://docs.python.org/fr/3.8/library/argparse.html)