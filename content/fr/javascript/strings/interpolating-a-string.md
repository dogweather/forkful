---
date: 2024-01-20 17:51:37.133763-07:00
description: "L'interpolation de cha\xEEnes permet d'ins\xE9rer des valeurs de variables\
  \ dans du texte. Les d\xE9veloppeurs l'utilisent pour rendre le code plus lisible\
  \ et\u2026"
lastmod: '2024-03-13T22:44:58.259632-06:00'
model: gpt-4-1106-preview
summary: "L'interpolation de cha\xEEnes permet d'ins\xE9rer des valeurs de variables\
  \ dans du texte. Les d\xE9veloppeurs l'utilisent pour rendre le code plus lisible\
  \ et\u2026"
title: "Interpolation de cha\xEEnes de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
L'interpolation de chaînes permet d'insérer des valeurs de variables dans du texte. Les développeurs l'utilisent pour rendre le code plus lisible et dynamique.

## How to (Comment faire :) 
Interpoler une chaîne en JavaScript est simple avec les littéraux de gabarits (template literals). Voici comment:

```Javascript
let prenom = "Alex";
let salutation = `Bonjour ${prenom}, comment vas-tu ?`;

console.log(salutation); // Affiche: Bonjour Alex, comment vas-tu ?
```

## Deep Dive (Plongée Profonde)
L'interpolation est arrivée avec ES6 (ECMAScript 2015), avant ça on concaténait avec `+`. Aujourd'hui, c'est obsolète. Pourquoi? Les littéraux de gabarits rendent le code plus propre et moins sujet aux erreurs. Auparavant, on aurait fait:

```Javascript
let prenom = "Alex";
let salutation = 'Bonjour ' + prenom + ', comment vas-tu ?';

console.log(salutation); // Affiche: Bonjour Alex, comment vas-tu ?
```

Mais imaginez une concaténation multiple avec beaucoup de variables et de chaînes. C'était un cauchemar à lire et à écrire. Avec l'interpolation, plus de soucis. De plus, les littéraux de gabarits gèrent aussi les retours à la ligne et les expressions, ce qui ajoute de la puissance :

```Javascript
let heure = 9;
let message = `Il est ${heure} heures ${
  heure > 12 ? "de l'après-midi" : "du matin"
}.`;

console.log(message); // Affiche: Il est 9 heures du matin.
```

Les alternatives modernes à cette approche, comme les fonctions de formatage de chaînes de certaines bibliothèques, offrent souvent des fonctionnalités plus avancées, mais dans la plupart des cas, les littéraux de gabarit ES6 suffisent.

## See Also (Voir Aussi)
Pour mieux comprendre l'interpolation et ce que vous pouvez faire avec :

- Documentation MDN sur les littéraux de gabarit : [MDN Template Literals](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Template_literals)
- Découvrir les nouveautés d'ES6, y compris l'interpolation de chaîne : [ES6 Features](http://es6-features.org/#StringInterpolation)
