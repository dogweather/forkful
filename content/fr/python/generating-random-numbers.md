---
title:    "Python: Génération de nombres aléatoires"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une tâche courante dans la programmation. Que vous ayez besoin de créer des données de test, de simuler des événements aléatoires ou d'ajouter un élément de surprise à votre programme, la génération de nombres aléatoires est une compétence précieuse à posséder.

## Comment faire

La génération de nombres aléatoires est facile à faire en Python grâce à la bibliothèque intégrée `random`. Voyons un exemple simple de génération de 5 nombres aléatoires entre 1 et 10 :

```Python
import random

for i in range(5):
  print(random.randint(1, 10))
```

Output :
```
3
7
9
1
5
```

## Plongée en profondeur

La fonction `randint()` de la bibliothèque `random` génère des nombres entiers aléatoires dans un intervalle donné. Mais saviez-vous qu'il existe d'autres fonctions pour générer des floats, des séquences aléatoires et même des choix aléatoires dans une liste ? La documentation officielle de Python a une section dédiée à la génération de nombres aléatoires pour en savoir plus sur ces différentes fonctions et leurs paramètres.

## Voir aussi

- [Documentation Python sur la génération de nombres aléatoires](https://docs.python.org/fr/3/library/random.html)
- [Article du blog Real Python sur la génération de nombres aléatoires en Python](https://realpython.com/python-random/)
- [Vidéo YouTube d'une présentation sur la bibliothèque random en Python](https://www.youtube.com/watch?v=NqEvSrqg_Hc)