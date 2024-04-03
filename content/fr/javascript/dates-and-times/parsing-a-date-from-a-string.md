---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:25.593833-07:00
description: "Comment : JavaScript propose nativement la m\xE9thode `Date.parse()`\
  \ et le constructeur `Date` pour analyser les cha\xEEnes de dates. Cependant, ces\
  \ approches\u2026"
lastmod: '2024-03-13T22:44:58.285713-06:00'
model: gpt-4-0125-preview
summary: "JavaScript propose nativement la m\xE9thode `Date.parse()` et le constructeur\
  \ `Date` pour analyser les cha\xEEnes de dates."
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
weight: 30
---

## Comment :
JavaScript propose nativement la méthode `Date.parse()` et le constructeur `Date` pour analyser les chaînes de dates. Cependant, ces approches présentent des limites et des incohérences entre différents navigateurs, notamment avec des formats de date non standards. Pour résoudre ces problèmes, des bibliothèques tierces comme `Moment.js` et `date-fns` sont populaires pour leur robustesse et leur facilité d'utilisation.

### Utiliser JavaScript natif :
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // Sortie : Sun Apr 30 2023 14:55:00 GMT+0000 (Heure universelle coordonnée)
```

### Utiliser Moment.js :
Tout d'abord, installez Moment.js via npm ou incluez-le dans votre projet. Ensuite :
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // Sortie : Sun Apr 30 2023 14:55:00 GMT+0000
```

### Utiliser date-fns :
Après avoir ajouté `date-fns` à votre projet, analysez une chaîne de date de la manière suivante :
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // Sortie : 2023-04-30T14:55:00.000Z
```

`Moment.js` et `date-fns` offrent des capacités d'analyse plus complètes, y compris la gestion d'une variété de formats et de locales, ce qui les rend préférables pour les applications complexes.
