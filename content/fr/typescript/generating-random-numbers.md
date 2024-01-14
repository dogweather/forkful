---
title:    "TypeScript: La génération de nombres aléatoires"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires en TypeScript?

La génération de nombres aléatoires est une fonctionnalité couramment utilisée dans la programmation. Elle peut être utilisée pour créer des jeux, générer des données aléatoires pour les tests ou tout simplement pour ajouter une variabilité à votre code. En TypeScript, il existe plusieurs méthodes pour générer des nombres aléatoires, en fonction de vos besoins et de votre environnement de développement.

## Comment générer des nombres aléatoires en TypeScript?

Pour générer des nombres aléatoires en TypeScript, nous pouvons utiliser la fonction Math.random(), qui renvoie un nombre aléatoire entre 0 (inclus) et 1 (exclus). Nous pouvons ensuite multiplier ce nombre par le nombre maximal que nous voulons obtenir et l'arrondir à l'aide de Math.floor() pour obtenir un nombre entier aléatoire.

```
TypeScript
const randomNumber = Math.floor(Math.random() * 100); // génère un nombre aléatoire entre 0 et 99
```

Nous pouvons également utiliser la fonction Math.round() pour obtenir un nombre entier arrondi au plus proche. Pour générer un nombre aléatoire dans une plage spécifique, nous pouvons utiliser la formule suivante:

```
TypeScript
const randomNumberInGivenRange = Math.floor(Math.random() * (max - min + 1)) + min;
```

Il existe également des bibliothèques TypeScript dédiées à la génération de nombres aléatoires, telles que Random.js et Chance.js, qui offrent une plus grande flexibilité et des fonctionnalités supplémentaires.

## Plongée en profondeur sur la génération de nombres aléatoires

La fonction Math.random() utilise un générateur de nombres pseudo-aléatoires (PRNG en anglais) pour créer des nombres aléatoires. Cela signifie que les nombres générés ne sont pas vraiment aléatoires, mais plutôt une séquence de nombres déterministe qui peut être reproduite si nécessaire. Pour obtenir des nombres vraiment aléatoires, nous devons utiliser des sources externes, telles que le temps ou les entrées utilisateur, comme seed (graine) pour le générateur PRNG.

Il est également important de noter que la génération de nombres aléatoires en TypeScript peut être différente selon l'environnement de développement dans lequel vous travaillez. Par exemple, lors de l'exécution du code dans un navigateur, il peut y avoir des différences dans la manière dont les PRNG sont initialisés et utilisés par rapport à l'exécution du code dans un environnement Node.js.

## Voir aussi

- [Documentation officielle TypeScript sur la fonction Math.random()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-9.html#mathrandom-seedable)
- [Bibliothèque Random.js](https://randomjs.com/)
- [Bibliothèque Chance.js](https://chancejs.com/)

Merci d'avoir lu cet article sur la génération de nombres aléatoires en TypeScript. Vous pouvez maintenant utiliser cette fonctionnalité pour rendre votre code plus intéressant et dynamique. N'hésitez pas à explorer d'autres méthodes et bibliothèques pour trouver celle qui correspond le mieux à vos besoins. Amusez-vous à coder en TypeScript !