---
title:    "Python: Lecture des arguments de ligne de commande"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Pourquoi lire les arguments de ligne de commande en Python?

Il est essentiel de savoir comment lire les arguments de ligne de commande lorsque vous programmez en Python, car cela peut être très utile pour créer des programmes interactifs et personnalisables. En utilisant des arguments de ligne de commande, vous pouvez permettre à vos utilisateurs de fournir des informations spécifiques lorsqu'ils exécutent votre programme, ce qui peut rendre votre code plus polyvalent et convivial.

## Comment faire pour lire les arguments de ligne de commande en Python

Pour lire les arguments de ligne de commande en Python, vous devez utiliser la bibliothèque `sys`, qui fait partie de la bibliothèque standard de Python. La méthode la plus courante consiste à utiliser la variable `argv`, qui stocke tous les arguments passés en ligne de commande.

Voici un exemple de code Python qui lit les arguments de ligne de commande et les affiche à l'écran :

```Python
import sys

# Stocke les arguments de ligne de commande dans une variable
arguments = sys.argv

# Affiche les arguments un par un
for arg in arguments:
    print(arg)
```

Si vous utilisez ce code et exécutez votre programme avec des arguments, vous devriez voir une sortie comme celle-ci :

```
python mon_programme.py arg1 arg2 arg3
```

```
mon_programme.py
arg1
arg2
arg3
```

## Plongée plus profonde dans la lecture des arguments de ligne de commande

En utilisant la méthode `argv`, vous aurez accès à une liste contenant tous les arguments fournis en ligne de commande. Mais que se passe-t-il si vous voulez traiter certaines de ces données comme des nombres, des chaînes de caractères, ou appliquer d'autres manipulations ? Pour ce faire, vous pouvez utiliser les méthodes `int()` et `float()` pour convertir les données en nombres, ainsi que d'autres méthodes pour manipuler les chaînes de caractères.

Par exemple, si vous voulez obtenir la somme de deux nombres fournis par l'utilisateur en tant qu'arguments de ligne de commande, vous pouvez utiliser le code suivant :

```Python
import sys

# Stocke les arguments de ligne de commande dans une variable
arguments = sys.argv

# Convertit les arguments en nombres entiers
nombre1 = int(arguments[1])
nombre2 = int(arguments[2])

# Calcule la somme et l'affiche à l'écran
somme = nombre1 + nombre2
print("La somme de ", nombre1, " et ", nombre2, " est ", somme)
```

Maintenant, si vous exécutez votre programme avec deux nombres comme arguments, vous obtiendrez une sortie comme celle-ci :

```
python mon_programme.py 5 7
```

```
La somme de 5 et 7 est 12
```

# Voir aussi

- [Documentation officielle de Python pour lire les arguments de ligne de commande](https://docs.python.org/fr/3/library/sys.html#sys.argv)
- [Un tutoriel complet sur l'utilisation des arguments de ligne de commande en Python](https://realpython.com/command-line-interfaces-python-argparse/)