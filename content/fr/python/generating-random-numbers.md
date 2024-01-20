---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi? 
La génération de nombres aléatoires est l'art de produire des nombres qui ne sont pas prévisibles. Les programmeurs la utilisent pour diverses raisons, notamment pour jouer à des jeux, faire de la simulation et tester la robustesse des programmes.

## Comment faire:
Voici comment générer des nombres aléatoires en utilisant Python.

```Python
import random

# Génère un nombre aléatoire entre 0 et 1
random_float = random.random()
print(random_float)

# Génère un nombre entier aléatoire entre 1 et 10
random_int = random.randint(1, 10)
print(random_int)
```

Quand vous exécutez ce script, vous obtiendrez un type de sortie comme celui ci-dessous. 

```Python
0.7392457751217108
4
```

## Plongée en profondeur
Historiquement, générer des nombres aléatoires était difficile car une machine, par définition, suit un ensemble d'instructions prédéterminé. Grace à des techniques modernes comme la méthode de Monte Carlo, nous pouvons maintenant générer des nombres aléatoires.

Comme alternative à la bibliothèque `random` de Python, vous pouvez utiliser `numpy.random`. Il a des fonctions similaires mais est plus rapide pour les grands ensembles de données.

Voici un exemple de comment utiliser `numpy.random`.

```Python
import numpy as np

# Génère un nombre aléatoire entre 0 et 1
random_float = np.random.random()
print(random_float)

# Génère un nombre entier aléatoire entre 1 et 10
random_int = np.random.randint(1, 10)
print(random_int)
```

L'implémentation réelle de la génération de nombres aléatoires en Python est basée sur l'algorithme de Mersenne Twister.

## Voir aussi
Pour en savoir plus sur la génération de nombres aléatoires en Python, consultez ces ressources:

1. La documentation officielle de Python sur le module `random`: https://docs.python.org/3/library/random.html

2. La documentation officielle de NumPy sur le module `random`: https://numpy.org/doc/stable/reference/random/index.html

3. Un article de Wikipedia expliquant l'algorithme de Mersenne Twister: https://fr.wikipedia.org/wiki/Mersenne_twister