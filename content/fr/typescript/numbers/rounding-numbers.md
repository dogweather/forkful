---
date: 2024-01-26 03:47:17.114965-07:00
description: "Comment faire : L'arrondi en TypeScript peut se faire en utilisant plusieurs\
  \ m\xE9thodes. Voici un rapide aper\xE7u ."
lastmod: '2024-03-13T22:44:57.431119-06:00'
model: gpt-4-0125-preview
summary: "L'arrondi en TypeScript peut se faire en utilisant plusieurs m\xE9thodes."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
L'arrondi en TypeScript peut se faire en utilisant plusieurs méthodes. Voici un rapide aperçu :

```typescript
// Math.round arrondit au plus proche entier
console.log(Math.round(1.5)); // Résultat : 2

// Math.ceil arrondit à l'entier supérieur
console.log(Math.ceil(1.1)); // Résultat : 2

// Math.floor arrondit à l'entier inférieur
console.log(Math.floor(1.8)); // Résultat : 1

// toFixed arrondit à un nombre fixe de décimales
let num = 1.23456;
console.log(num.toFixed(2)); // Résultat : "1.23"
// Note : toFixed renvoie une chaîne de caractères ! Utilisez parseFloat pour reconvertir si nécessaire.
console.log(parseFloat(num.toFixed(2))); // Résultat : 1.23
```

## Plongée en profondeur
Autrefois, l'arrondi était indispensable en raison de l'espace limité et des problèmes de précision des premiers ordinateurs. Aujourd'hui, l'arithmétique à virgule flottante peut entraîner des résultats étranges en raison de la manière dont les nombres sont stockés en binaire. Les alternatives à l'arrondi incluent floor, ceil, et trunc (pour couper les décimales sans arrondir).

Les mécanismes internes sont à noter : `Math.round` suit la règle de "l'arrondi à la demi-supérieure" (aussi connu sous le nom d'"arrondi commercial"), tandis que `Math.floor` et `Math.ceil` sont plus directs. `toFixed` peut provoquer des résultats inattendus car il retourne une chaîne de caractères, et il arrondit selon la méthode "arrondir à la demi-pair" (aussi connu sous le nom d'"arrondi bancaire"), particulièrement utile pour réduire le biais lors de l'arrondi de mêmes nombres plusieurs fois.

## Voir aussi
- [MDN - Math.round()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
