---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:47.414028-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Générer des nombres aléatoires en programmation, c'est comme tirer des billes d'un sac à l'aveugle. Les développeurs s'en servent pour tout, des jeux vidéo à la cryptographie, en passant par les simulations et tests d'algorithmes.

## How to (Comment faire) :
Python offre le module `random` pour gérer le hasard. Voici comment l'utiliser :

```python
import random

# Générer un nombre aléatoire entre 1 et 100
nombre_aleatoire = random.randint(1, 100)
print(nombre_aleatoire)
```

Sortie possible : `42` (mais ça, c'est au pif, hein!)

Pour des flottants, utilisez `random.random()` :

```python
# Générer un flottant aléatoire entre 0 et 1
flottant_aleatoire = random.random()
print(flottant_aleatoire)
```

Sortie possible : `0.8574624` (encore une fois, c'est aléatoire!)

## Deep Dive (Plongée en profondeur) :
L'histoire des nombres aléatoires est fascinante. Avant, on utilisait des dés, des pièces, mais en informatique, on a besoin de méthodes plus rapides et automatisées. Si `random` de Python nous semble magique, ce sont en réalité des algorithmes déterministes, pas de vrais hasards - on parle de pseudo-aléatoire. 

Le module `random` utilise par défaut l'algorithme Mersenne Twister, qui est un bon compromis entre vitesse et complexité pour beaucoup d'applications. Mais pour la crypto, gaffe, il n'est pas assez sécurisé. Python offre l'alternative `secrets` pour ça.

Les nombres "vraiment" aléatoires peuvent être générés par des phénomènes physiques imprédictibles, mais c'est une autre histoire.

## See Also (Voir aussi) :
- La documentation officielle du module `random` : https://docs.python.org/3/library/random.html
- Le module `secrets` pour une sécurité accrue : https://docs.python.org/3/library/secrets.html
- Un aperçu de l'algorithme Mersenne Twister pour les curieux : https://en.wikipedia.org/wiki/Mersenne_Twister