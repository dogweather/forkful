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

## Qu'est-ce que c'est et pourquoi?
Générer des nombres aléatoires est une pratique courante en programmation qui consiste à générer des nombres aléatoires dans un intervalle spécifié. Les programmeurs le font pour diverses raisons, notamment pour créer des simulations ou des jeux, pour sécuriser des mots de passe et pour tester des algorithmes.

## Comment:
Voici un exemple de code Python pour générer un nombre aléatoire entre 1 et 10 et l'imprimer:

```Python
from random import randint

nbre_aleatoire = randint(1, 10)
print(nbre_aleatoire)
```

Output:

```
7
```

## Plongée en profondeur:
La génération de nombres aléatoires existe depuis des siècles, avec des méthodes telles que le lancer de dés. En programmation, il existe différentes façons de générer des nombres aléatoires, comme utiliser des algorithmes ou des générateurs pseudo-aléatoires. Il est important de garder à l'esprit que ces nombres ne sont pas vraiment aléatoires, mais ils sont déterministes dans leur nature.

## Voir aussi:
- [Documentation officielle de Python sur la génération de nombres aléatoires](https://docs.python.org/fr/3/library/random.html)
- [Article Wikipédia sur la génération de nombres aléatoires](https://fr.wikipedia.org/wiki/Nombre_al%C3%A9atoire)
- [Vidéo YouTube expliquant comment fonctionnent les générateurs pseudo-aléatoires en informatique](https://www.youtube.com/watch?v=GtOt7EBNEwQ)