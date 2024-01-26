---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:15.650570-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres aléatoires en JavaScript, c'est comme jeter des dés en ligne de code. On en a besoin pour tout ce qui doit être imprévisible : jeux, simulations, tests ou sécurité informatique.

## Comment faire :
Pour tirer un nombre au hasard entre 0 (inclus) et 1 (exclus) :

```Javascript
let random = Math.random();
console.log(random); // 0.54321 par exemple
```

Pour obtenir un nombre aléatoire entre deux valeurs, min et max :

```Javascript
function getRandomArbitrary(min, max) {
  return Math.random() * (max - min) + min;
}
console.log(getRandomArbitrary(1, 10)); // 6.4321 par exemple
```

Si tu veux un entier, pas des décimales :

```Javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
console.log(getRandomInt(1, 10)); // 4 par exemple
```

## Immersion :
Historiquement, la génération de nombres aléatoires en informatique a involu dès les premiers ordinateurs. Le vrai "aléatoire" est quasi-impossible à obtenir par des machines, donc on utilise des algorithmes pour des approximations.

Les méthodes changent selon les besoins. Parfois, tu veux du vrai aléa, dit "cryptographiquement sécurisé". Pour ça, en JavaScript, tu peux utiliser `crypto.getRandomValues()`. C'est plus complexe, surtout si tu débutes, mais bon à savoir.

Détail d'implémentation : `Math.random()` utilise un générateur de nombres pseudo-aléatoires. Ce n'est pas idéal pour la sécurité car le processus est en fait prévisible avec assez d'infos. Mais pour la plupart des usages, c'est suffisant.

## Voir aussi :
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs - crypto.getRandomValues()](https://developer.mozilla.org/fr/docs/Web/API/Crypto/getRandomValues)
