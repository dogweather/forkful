---
title:                "Génération de nombres aléatoires"
html_title:           "Python: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser des nombres aléatoires?

Générer des nombres aléatoires est utile dans de nombreux cas de programmation, tels que la simulation de données, la génération de mots de passe sécurisés ou même dans les jeux. Cela peut également être utile pour tester un programme en lui fournissant différents scénarios aléatoires.

## Comment faire?

Il existe différentes manières de générer des nombres aléatoires en Python, voici quelques exemples :

```Python
# Méthode 1 : Utiliser le module random pour générer un nombre entier entre 0 et 10

import random

num = random.randint(0, 10)
print(num)

# Méthode 2 : Utiliser le module secrets pour générer un nombre entier entre 100 et 200

import secrets

num = secrets.randbelow(101) + 100
print(num)

# Méthode 3 : Utiliser le module numpy pour générer un nombre décimal entre 0 et 1

import numpy as np

num = np.random.rand()
print(num)
```

Output:

```
7
187
0.523619775
```

## Approfondissement

Python utilise un algorithme appelé Mersenne Twister pour générer des nombres aléatoires. Cet algorithme est basé sur des opérations mathématiques complexes et prend comme paramètres une "graine" initiale et une période. La période est le nombre de valeurs différentes que l'algorithme peut générer avant de répéter un nombre. La graine détermine le point de départ du générateur de nombres aléatoires.

Il est également possible de définir une graine manuellement en utilisant la fonction `random.seed()`. Cela peut être utile si vous souhaitez reproduire exactement la même séquence de nombres aléatoires à chaque exécution de votre programme.

## Voir aussi

- La documentation officielle sur les modules random et secrets : https://docs.python.org/fr/3/library/random.html https://docs.python.org/fr/3/library/secrets.html
- La documentation officielle sur le module numpy : https://numpy.org/doc/
- Un tutoriel sur la génération de nombres aléatoires en Python : https://realpython.com/python-random/