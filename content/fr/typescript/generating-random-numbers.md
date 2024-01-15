---
title:                "La génération de nombres aléatoires"
html_title:           "TypeScript: La génération de nombres aléatoires"
simple_title:         "La génération de nombres aléatoires"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler être une activité amusante et ludique, mais en réalité, c'est une fonctionnalité importante pour de nombreux programmes. Que ce soit pour créer des simulations, des jeux ou des tests, l'utilisation de nombres aléatoires peut rendre votre code plus réaliste et efficace.

## Comment faire

Pour générer des nombres aléatoires en TypeScript, il existe plusieurs méthodes intégrées à la langue que vous pouvez utiliser. Voici un exemple simple pour générer un nombre entier aléatoire compris entre 1 et 10 :

```TypeScript
// Importation de la fonction Math.random() qui renvoie un nombre compris entre 0 (inclus) et 1 (exclu)
// Multipliez-le par 10 pour obtenir un nombre entre 0 (inclus) et 10 (exclu)
// Utilisez Math.floor() pour arrondir à l'entier inférieur le plus proche
// Ajoutez 1 pour obtenir des nombres entre 1 et 10
const randomNumber = Math.floor(Math.random() * 10) + 1;
```

Vous pouvez également utiliser la fonction Math.round() pour obtenir un nombre aléatoire décimal, ou spécifier une plage différente en ajustant les valeurs dans l'exemple ci-dessus.

Pour générer plusieurs nombres aléatoires, vous pouvez utiliser une boucle ou une fonction récursive pour appeler la fonction de génération à plusieurs reprises.

## Plongée en profondeur

Vous vous demandez peut-être comment fonctionne exactement la fonction Math.random() pour générer des nombres aléatoires en TypeScript. En réalité, cette fonction utilise un algorithme complexe appelé générateur de nombres pseudo-aléatoires (PRNG en anglais) pour produire des séquences de nombres qui semblent aléatoires. Les PRNG sont basés sur une "graine" de départ, qui peut être définie manuellement ou générée automatiquement par l'ordinateur. Cela signifie que si vous utilisez la même "graine", vous obtiendrez la même séquence de nombres aléatoires. De plus, les PRNG ne sont pas véritablement aléatoires, car ils suivent un modèle prévisible, mais pour la plupart des cas d'utilisation, ils peuvent être considérés comme suffisamment aléatoires.

## Voir aussi

- La documentation officielle de TypeScript sur la fonction Math.random() : https://www.typescriptlang.org/docs/handbook/standard-library.html#math
- Un article sur les générateurs de nombres aléatoires en informatique : https://en.wikipedia.org/wiki/Random_number_generation