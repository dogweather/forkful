---
date: 2024-01-20 17:39:25.300864-07:00
description: "Convertir une cha\xEEne en minuscules, c'est transformer tous les caract\xE8\
  res alphab\xE9tiques en leur \xE9quivalent en minuscule. Les programmeurs le font\
  \ pour\u2026"
lastmod: '2024-03-13T22:44:57.422863-06:00'
model: gpt-4-1106-preview
summary: "Convertir une cha\xEEne en minuscules, c'est transformer tous les caract\xE8\
  res alphab\xE9tiques en leur \xE9quivalent en minuscule."
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

## What & Why?
Convertir une chaîne en minuscules, c'est transformer tous les caractères alphabétiques en leur équivalent en minuscule. Les programmeurs le font pour standardiser les entrées des utilisateurs, faciliter les comparaisons de chaînes et l'indexation de données sans se soucier de la casse.

## How to:
En TypeScript, c'est simple comme bonjour :

```TypeScript
let greeting: string = "Bonjour LE MONDE!";
let lowerCaseGreeting: string = greeting.toLowerCase();
console.log(lowerCaseGreeting); // affiche "bonjour le monde!"
```

Si on veut transformer une chaîne en direct :

```TypeScript
console.log("ÇA VA?".toLowerCase()); // affiche "ça va?"
```

## Deep Dive
Historiquement, la méthode `toLowerCase()` existe en JavaScript depuis ses premières versions, et TypeScript, en tant que surcouche, l’a conservée. Cette constance aide les développeurs à écrire du code de manière prévisible et fiable.

Alternativement, on pourrait considérer l’usage de `toLocaleLowerCase()`, qui est sensée aux spécificités locales. Par exemple, en turc, la lettre 'I' majuscule se transforme en 'ı' (sans point) en minuscule.

Pour la petite histoire, vous pouvez essayer :

```TypeScript
console.log("I".toLocaleLowerCase("tr-TR")); // affiche "ı"
```

Côté moteur JavaScript, `toLowerCase()` travaille au niveau de l'unicode et suit les spécifications de `EcmaScript`.

## See Also
Pour aller plus loin, consultez :
- La documentation Mozilla sur `toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase)
- La spécification ECMAScript pour comprendre les fondements : [ECMAScript Language Specification](https://www.ecma-international.org/ecma-262/6.0/#sec-string.prototype.tolowercase)
