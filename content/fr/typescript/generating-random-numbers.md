---
title:                "TypeScript: Création de nombres aléatoires"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est utile pour créer des simulations, des jeux ou tout autre programme nécessitant une composante aléatoire. Cela permet également de tester le comportement de certains algorithmes dans des conditions variées.

## Comment faire

Générer un nombre aléatoire en TypeScript est facile grâce à la fonction `Math.random()`. Cette fonction renvoie un nombre compris entre 0 et 1 (exclus). En multipliant ce nombre par un autre nombre et en utilisant `Math.floor()` pour arrondir à l'entier inférieur, vous pouvez générer un nombre aléatoire dans une plage définie.

```
TypeScript
// Générer un nombre aléatoire entre 1 et 10
const randomNumber = Math.floor(Math.random() * 10) + 1;

console.log(randomNumber); // Output: Un nombre aléatoire entre 1 et 10
```

## Plongée en profondeur

Il est important de noter que la fonction `Math.random()` ne génère pas réellement des nombres aléatoires, mais plutôt une séquence pseudo-aléatoire basée sur une graine interne. Cela signifie que si vous appelez la fonction à plusieurs reprises, vous obtiendrez la même séquence de nombres. Pour éviter cela, vous pouvez utiliser `Math.random()` pour générer une graine aléatoire pour d'autres fonctions de génération de nombres aléatoires plus complexes.

De plus, il existe d'autres bibliothèques et modules en TypeScript qui offrent un contrôle plus avancé sur la génération de nombres aléatoires, tels que `random-js` ou `seedrandom`.

## Voir aussi

- [Documentation officielle TypeScript sur Math.random()](https://www.typescriptlang.org/docs/handbook/stdlib.html#mathrandom)
- [Exemple de génération de nombres aléatoires en TypeScript](https://codepen.io/kirbyedy/pen/NWgoVOr?editors=0012)
- [Bibliothèque random-js pour générer des nombres aléatoires en TypeScript](https://github.com/ckknight/random-js)