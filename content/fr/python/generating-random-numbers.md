---
title:                "Python: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de vous demander pourquoi générer des nombres aléatoires est une compétence utile en programmation. Eh bien, il existe de nombreuses raisons pour lesquelles vous pourriez avoir besoin de générer des nombres aléatoires dans votre code. Par exemple, les jeux utilisent souvent des nombres aléatoires pour créer des situations différentes à chaque fois que vous jouez. Ou bien, vous pourriez avoir besoin de générer des données de test pour vos applications. Quelle que soit la raison, la génération de nombres aléatoires est une compétence importante à avoir dans votre boîte à outils de programmation.

## Comment faire

Voici un exemple de code Python simple pour générer un nombre aléatoire entre 1 et 10 :

```Python
import random

nombre_aleatoire = random.randint(1, 10)
print(nombre_aleatoire)
```

Cela générera un nombre aléatoire chaque fois que vous exécutez le code. Vous pouvez également utiliser des fonctions comme `random.randrange()` ou `random.uniform()` pour générer des nombres aléatoires dans des plages spécifiques ou avec une précision décimale.

## Plongée en profondeur

Maintenant que vous savez comment générer des nombres aléatoires en Python, vous pourriez vous demander comment cela fonctionne réellement. En termes simples, les ordinateurs ne peuvent pas générer de véritables nombres aléatoires. Au lieu de cela, ils utilisent des algorithmes pour produire des nombres qui apparaissent aléatoires. Ces algorithmes se basent sur une "graine" initiale, qui est un nombre de départ à partir duquel les nombres aléatoires sont générés. Si vous utilisez la même graine, vous obtiendrez toujours la même série de nombres aléatoires.

## Voir aussi

- [La documentation officielle de Python pour la génération de nombres aléatoires](https://docs.python.org/fr/3/library/random.html)
- [Un tutoriel sur la génération de nombres aléatoires en Python](https://realpython.com/python-random/)
- [Les risques de sécurité associés à la génération de nombres aléatoires](https://www.owasp.org/index.php/Randomness_and_Security)