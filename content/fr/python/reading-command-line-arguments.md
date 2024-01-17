---
title:                "La lecture des arguments de ligne de commande"
html_title:           "Python: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Lire les arguments de ligne de commande est une technique utilisée par les programmeurs pour obtenir des informations de l'utilisateur lors de l'exécution d'un programme. Cela permet aux utilisateurs de personnaliser l'exécution du programme en fournissant différentes valeurs en fonction de leurs besoins.

## Comment faire:

Voici un exemple simple qui utilise la méthode `sys.argv` pour lire les arguments de ligne de commande dans un programme Python:

```python
import sys

# la première valeur de sys.argv est toujours le nom du fichier python lui-même
# les arguments suivants sont ensuite stockés dans un tableau
arguments = sys.argv[1:]
nombre_arguments = len(arguments)

print("Il y a " + str(nombre_arguments) + " arguments de ligne de commande:")
for argument in arguments:
    print(argument)
```

Si vous exécutez ce programme avec la commande `python arguments.py hello world`, vous obtiendrez le résultat suivant:

```
Il y a 2 arguments de ligne de commande:
hello
world
```

## Plongée en profondeur:

La technique de lecture d'arguments de ligne de commande est couramment utilisée dans les langages de programmation, notamment en C, C++, Java et bien sûr en Python. Avant l'apparition de cette méthode, les programmeurs devaient utiliser des bibliothèques spécifiques à leur système d'exploitation pour lire les arguments de ligne de commande.

Une alternative à la méthode `sys.argv` est l'utilisation du module `argparse`, qui offre une plus grande flexibilité et une meilleure gestion des erreurs lors de la lecture des arguments de ligne de commande.

En termes d'implémentation, la méthode `sys.argv` utilise deux variables spéciales: `sys.argv` pour stocker tous les arguments et `sys.argc` pour compter le nombre total d'arguments.

## Voir aussi:

Vous pouvez en savoir plus sur la méthode `sys.argv` et découvrir d'autres techniques pour lire les arguments de ligne de commande en consultant les ressources suivantes:

- [Documentation de Python sur sys.argv](https://docs.python.org/fr/3.9/library/sys.html#sys.argv)
- [Documentation de Python sur argparse](https://docs.python.org/fr/3.9/howto/argparse.html)