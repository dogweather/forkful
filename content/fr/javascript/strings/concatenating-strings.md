---
date: 2024-01-20 17:35:24.579938-07:00
description: "La concat\xE9nation de cha\xEEnes est le processus de raccordement de\
  \ deux cha\xEEnes de texte ou plus en une seule. Les programmeurs le font pour construire\
  \ des\u2026"
lastmod: '2024-03-13T22:44:58.266209-06:00'
model: gpt-4-1106-preview
summary: "La concat\xE9nation de cha\xEEnes est le processus de raccordement de deux\
  \ cha\xEEnes de texte ou plus en une seule. Les programmeurs le font pour construire\
  \ des\u2026"
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## What & Why?
La concaténation de chaînes est le processus de raccordement de deux chaînes de texte ou plus en une seule. Les programmeurs le font pour construire des messages dynamiques, des requêtes SQL, ou toute structure de données nécessitant du texte combiné.

## How to:
Pour concaténer des chaînes en JavaScript, on utilise souvent l'opérateur `+` ou la méthode `concat()`.

```javascript
let salutation = "Bonjour";
let nom = "Alice";
let message = salutation + ", " + nom + "!"; // Utilisation de +
console.log(message); // "Bonjour, Alice!"

// Alternative avec concat()
message = salutation.concat(", ", nom, "!");
console.log(message); // "Bonjour, Alice!"
```

Avec les littéraux de gabarit (template literals), concaténer devient encore plus simple.

```javascript
message = `${salutation}, ${nom}!`;
console.log(message); // "Bonjour, Alice!"
```

## Deep Dive
Historiquement, la concaténation était réalisée avec l'opérateur `+`. En ES6 (ECMAScript 2015), JavaScript introduit les littéraux de gabarit avec des backticks (\`) qui permettent d'insérer des variables directement dans le string et peuvent rendre le code plus lisible.

Pour des opérations plus lourdes, `concat()` peut être moins performante que `+` ou les littéraux de gabarit du fait de la création d'une nouvelle chaîne à chaque appel. Les moteurs JavaScript optimisent souvent la concaténation avec `+` lors de l'exécution, particulièrement pour des concats simples.

Alternativement, pour concaténer de grands ensembles de chaînes ou dans une boucle, on pourrait envisager d'utiliser un array avec la méthode `join()` pour éviter les problèmes de performance.

```javascript
let mots = ["Bonjour", "Alice", "!"];
message = mots.join(", "); // Joins all array elements into a string
console.log(message); // "Bonjour, Alice, !"
```

## See Also
- MDN Web Docs: [String.prototype.concat()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/concat)
- MDN Web Docs: [Template literals (Template strings)](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Template_literals)
- You Might Not Need jQuery [String concatenation](http://youmightnotneedjquery.com/#concatenate)
