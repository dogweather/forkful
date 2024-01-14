---
title:                "Python: Génération de nombres aléatoires"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est une compétence importante pour tout programmeur Python. Elle permet de générer des jeux de données, de tester des fonctions et de simuler des situations aléatoires pour les programmes.

## Comment faire

Pour générer des nombres aléatoires en Python, vous pouvez utiliser la bibliothèque "random" intégrée. Voici un exemple de code qui génère 5 nombres aléatoires entre 1 et 10 :

```Python
import random
for i in range(5):
  print(random.randint(1,10))
```

La sortie de ce code pourrait ressembler à ceci :

```
4
9
2
8
3
```

## Plongée en profondeur

Le module "random" en Python propose de nombreuses fonctions utiles pour générer des nombres aléatoires. En plus de "randint", il existe également "randrange" pour générer des nombres aléatoires avec un pas donné, "uniform" pour générer un nombre aléatoire à virgule entre deux valeurs et "choice" pour choisir un élément aléatoire dans une liste.

Vous pouvez également utiliser la fonction "seed" pour initialiser la séquence de nombres aléatoires et ainsi obtenir des résultats reproductibles. Ceci est particulièrement utile pour le débogage et la reproductibilité des résultats.

## Voir aussi

- [Documentation de la bibliothèque random en Python](https://docs.python.org/fr/3/library/random.html)
- [Tutorial sur la génération de nombres aléatoires en Python](https://www.tutorialspoint.com/python/python_random_numbers.htm)
- [Article sur l'utilisation de nombres aléatoires en simulation informatique](https://fr.wikipedia.org/wiki/Nombres_al%C3%A9atoires_en_simulation_informatique)