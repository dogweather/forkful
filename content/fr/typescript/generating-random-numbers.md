---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Générer des nombres aléatoires en programmation permet de créer des données imprévisibles. C'est crucial pour des tâches comme les tests, l'encryption, et les jeux.

## Comment faire:
Voici un petit exemple montrant comment générer un nombre aléatoire en TypeScript. 

```TypeScript
function obtenirNombreAleatoire(max: number): number {
  return Math.floor(Math.random() * max);
}

console.log(obtenirNombreAleatoire(10));
```
Ce code génère un nombre aléatoire entre 0 et 9.

## Plongée Profonde
Historiquement, générer des nombres véritablement aléatoires en informatique a toujours été un défi. Les algos sont souvent pseudo-aléatoires, dépendant d'une «semence» initiale.

Pour générer des nombres plus aléatoires, des techniques plus avancées comme l'exploitation de la bruit quantique sont utilisées, mais généralement, `Math.random()` suffit pour la plupart des utilisations.

En parlant de `Math.random()`, c'est une fonction qui retourne un nombre à virgule flottante pseudo-aléatoire dans la plage [0, 1). Multiplicez le résultat par le nombre de valeurs maximum désiré, puis utilisez floor pour arrondir au nombre entier le plus proche.

## Voir Aussi
- [Mozilla Developer Network Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Introduction à la génération de nombres aléatoires - Towards Data Science](https://towardsdatascience.com/introduction-to-random-number-generation-2383c92a67a)