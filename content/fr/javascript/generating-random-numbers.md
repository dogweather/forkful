---
title:                "Génération de nombres aléatoires"
html_title:           "Javascript: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler inutile, mais cela peut en fait être très utile dans de nombreuses situations en programmation. Cela peut être utilisé pour générer des simulations, des jeux, des tests de performances, et bien plus encore. 

## Comment faire

Il existe plusieurs façons de générer des nombres aléatoires en Javascript. Voici quelques exemples de code pour vous aider à démarrer : 

```Javascript
// Génère un nombre entier aléatoire entre 1 et 10
let random1 = Math.floor(Math.random() * 10) + 1;
console.log(random1); // Output: un nombre entier aléatoire entre 1 et 10

// Génère un nombre à virgule aléatoire entre 0 et 1
let random2 = Math.random();
console.log(random2); // Output: un nombre à virgule aléatoire entre 0 et 1

// Génère un nombre entier aléatoire entre 50 et 100
let random3 = Math.floor(Math.random() * 51) + 50;
console.log(random3); // Output: un nombre entier aléatoire entre 50 et 100
```

Il est également possible d'utiliser une librairie comme Lodash pour simplifier le processus de génération de nombres aléatoires :

```Javascript
// Génère un nombre entier aléatoire entre 1 et 100
let random = _.random(1, 100);
console.log(random); // Output: un nombre entier aléatoire entre 1 et 100
```

## Plongée en profondeur

La méthode la plus courante pour générer des nombres aléatoires en Javascript est d'utiliser la fonction `Math.random()`. Cette fonction retourne un nombre à virgule aléatoire entre 0 et 1. Vous pouvez ensuite multiplier ce nombre par un certain nombre pour obtenir un résultat dans la plage souhaitée. Par exemple, si vous voulez générer un nombre aléatoire entre 1 et 10, vous multiplierez le résultat de `Math.random()` par 10 et vous arrondirez le résultat à l'aide de `Math.floor()` pour obtenir un nombre entier.

Cependant, la méthode `Math.random()` n'est pas vraiment aléatoire car elle utilise un algorithme pour générer les nombres. Pour obtenir une plus grande aléatoire, vous pouvez utiliser la fonction `Math.random()` pour générer un nombre entier aléatoire et l'utiliser comme graine pour une fonction de hachage. Cette approche est généralement utilisée pour les simulations et les jeux.

## Voir aussi

- [Documentation de Math.random() sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Math/random)
- [Documentation de Lodash sur la génération de nombres aléatoires](https://lodash.com/docs/4.17.15#random)