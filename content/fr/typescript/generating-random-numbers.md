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

## Qu'est-ce que c'est et pourquoi?
La génération de nombres aléatoires est une technique couramment utilisée par les programmeurs pour générer des valeurs aléatoires dans leurs applications. Cela peut être utile pour simuler des données, créer des éléments aléatoires tels que des jeux de hasard ou tout autre cas où des valeurs aléatoires sont nécessaires.

## Comment faire:
Voici un exemple de code en TypeScript pour générer un nombre aléatoire entre 0 et 1 et l'afficher dans la console:

```
const randomNumber = Math.random();
console.log(randomNumber);
```

Cet exemple utilise la fonction native `Math.random()` qui retourne un nombre aléatoire entre 0 (inclus) et 1 (exclu). Pour générer un nombre aléatoire dans une plage spécifique, vous pouvez utiliser la formule suivante:

```
const min = 10;
const max = 20;
const randomNumber = Math.random() * (max - min) + min;
console.log(randomNumber);
```

Dans cet exemple, le nombre aléatoire sera compris entre 10 (inclus) et 20 (exclu).

## Plongée en profondeur:
La génération de nombres aléatoires a été utilisée depuis des siècles pour diverses applications, allant des jeux de hasard aux simulations scientifiques. Auparavant, ils étaient générés à l'aide de méthodes telles que le lancer de dés ou la sélection de cartes. Avec l'avènement de l'informatique, des algorithmes ont été développés pour générer des nombres pseudo-aléatoires, qui apparaissent comme aléatoires mais sont en réalité déterminés par un calcul.

En plus de la fonction `Math.random()`, TypeScript propose également une bibliothèque de nombres aléatoires plus avancée appelée `Random.js`. Cette bibliothèque permet de générer des nombres aléatoires selon différentes distributions de probabilité.

Les programmeurs doivent être conscients que les nombres générés par des algorithmes sont en réalité pseudo-aléatoires et peuvent être prévisibles. Par conséquent, pour les applications nécessitant une sécurité ou une véritable aléatoire, il est préférable d'utiliser des sources externes telles que des capteurs de bruit thermique ou des mouvements de souris pour générer des nombres aléatoires.

## Voir aussi:
- [Documentation officielle de TypeScript sur la génération de nombres aléatoires](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#randomness)
- [Random.js documentation](https://github.com/ckknight/random-js/)
- [Article Wikipedia sur la génération de nombres aléatoires](https://fr.wikipedia.org/wiki/Génération_de_nombres_aléatoires)