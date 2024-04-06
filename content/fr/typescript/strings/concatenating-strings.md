---
date: 2024-01-20 17:35:36.428529-07:00
description: "How to: Historiquement, la concat\xE9nation de cha\xEEnes \xE9tait r\xE9\
  alis\xE9e principalement avec l'op\xE9rateur `+`. Mais cela pouvait devenir lourd\
  \ avec l'ajout de\u2026"
lastmod: '2024-04-05T22:51:11.521576-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la concat\xE9nation de cha\xEEnes \xE9tait r\xE9alis\xE9\
  e principalement avec l'op\xE9rateur `+`."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## How to:
Concaténer avec le signe `+` :

```TypeScript
let bonjour: string = "Bonjour";
let monde: string = "monde!";
let salutation: string = bonjour + " " + monde; // "Bonjour monde!"
```

Utiliser les littéraux de gabarits (template literals) avec les backticks ``` ` ``` :

```TypeScript
let prenom: string = "Jean";
let nom: string = "Dupont";
let fullName: string = `${prenom} ${nom}`; // "Jean Dupont"
```

Résultats :

```TypeScript
console.log(salutation); // Affiche "Bonjour monde!"
console.log(fullName);   // Affiche "Jean Dupont"
```

## Deep Dive
Historiquement, la concaténation de chaînes était réalisée principalement avec l'opérateur `+`. Mais cela pouvait devenir lourd avec l'ajout de variables et de chaînes complexes.

Avec l'ES6, TypeScript a hérité des littéraux de gabarits qui rendent le code plus lisible. Ils permettent aussi l'interpolation et le multiligne, ce qui était auparavant plus compliqué.

Il faut noter que sous le capot, lorsque vous utilisez l'opérateur `+`, TypeScript (et JavaScript) convertit les opérandes en chaînes si l'un d'eux est une chaîne de caractères, pouvant conduire à des comportements inattendus si vous n'êtes pas prudent.

Alternatives? Vous pouvez aussi concaténer avec les méthodes `concat()` ou des fonctions plus évoluées comme les tableaux et la méthode `join()`, mais pour commencer, `+` et les littéraux de gabarits sont vos meilleurs alliés.

## See Also
- [MDN Web Docs: Template literals](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Documentation: String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [ECMAScript 6 Features: Template Strings](http://es6-features.org/#StringInterpolation)
