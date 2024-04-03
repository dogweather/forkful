---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:39.634245-07:00
description: "Comment faire : En JavaScript, il n'existe pas de m\xE9thode int\xE9\
  gr\xE9e pour mettre directement en capitale les cha\xEEnes de caract\xE8res, mais\
  \ c'est simple \xE0\u2026"
lastmod: '2024-03-13T22:44:58.256084-06:00'
model: gpt-4-0125-preview
summary: "En JavaScript, il n'existe pas de m\xE9thode int\xE9gr\xE9e pour mettre\
  \ directement en capitale les cha\xEEnes de caract\xE8res, mais c'est simple \xE0\
  \ impl\xE9menter en utilisant les m\xE9thodes de manipulation de cha\xEEnes de base."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :
En JavaScript, il n'existe pas de méthode intégrée pour mettre directement en capitale les chaînes de caractères, mais c'est simple à implémenter en utilisant les méthodes de manipulation de chaînes de base.

### Utiliser le JavaScript Standard
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Sortie: "Hello world"
```

### Version ES6
Avec les littéraux de gabarits ES6, la fonction peut être écrite de manière plus succincte :
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Sortie: "Hello ES6"
```

### Utiliser Lodash
Lodash est une bibliothèque d'utilitaires tiers populaire qui offre une large gamme de fonctions pour manipuler et travailler avec les valeurs JavaScript, y compris les chaînes de caractères. Pour mettre une chaîne en capitale en utilisant Lodash :
```javascript
// D'abord, installez lodash si vous ne l'avez pas fait : npm install lodash
const _ = require('lodash');

console.log(_.capitalize('EXEMPLE LODASH')); // Sortie: "Lodash exemple"
```
_Remarquez comment Lodash non seulement met la première lettre en majuscule mais aussi convertit le reste de la chaîne en minuscule, ce qui diffère légèrement de l'implémentation en JavaScript pur._

### Utiliser CSS (Uniquement à des Fins d'Affichage)
Si l'objectif est de mettre le texte en capitale pour l'affichage dans l'UI, le CSS peut être utilisé :
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">bonjour css</div> <!-- Affiche "Bonjour css" -->
```
**Note :** Cette méthode change l'apparence du texte sur la page web sans modifier la chaîne elle-même en JavaScript.
