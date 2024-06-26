---
date: 2024-01-20 17:31:12.060239-07:00
description: "Comment faire : Historiquement, g\xE9rer le temps avec JavaScript pouvait\
  \ \xEAtre frustrant \xE0 cause des incoh\xE9rences et du manque de fonctionnalit\xE9\
  s. Des\u2026"
lastmod: '2024-04-05T21:53:59.691391-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, g\xE9rer le temps avec JavaScript pouvait \xEAtre frustrant\
  \ \xE0 cause des incoh\xE9rences et du manque de fonctionnalit\xE9s."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
```javascript
// Date actuelle
const maintenant = new Date();

// Calculer une date 10 jours dans le futur
const dixJoursPlusTard = new Date(maintenant.getTime() + 10 * 24 * 60 * 60 * 1000);
console.log(dixJoursPlusTard); // Affiche la date 10 jours plus tard

// Calculer une date 5 ans dans le passé
const cinqAnsPlusTot = new Date(maintenant.getTime() - 5 * 365.25 * 24 * 60 * 60 * 1000);
console.log(cinqAnsPlusTot); // Affiche la date 5 ans plus tôt
```

## Immersion :
Historiquement, gérer le temps avec JavaScript pouvait être frustrant à cause des incohérences et du manque de fonctionnalités. Des bibliothèques comme Moment.js étaient alors incontournables mais elles perdent du terrain maintenant grâce aux améliorations des navigateurs et au langage lui-même. 

Pour des calculs simples, `Date` est amplement suffisant mais attention aux années bissextiles lors de calculs sur de longues périodes. Pour plus de précision, les librairies comme `date-fns` ou `Luxon` sont recommandées car elles gèrent mieux les subtilités du temps. 

JavaScript utilise le temps universel coordonné (UTC) pour créer des dates, mais il est facile de se retrouver avec des problèmes de fuseau horaire si ce n'est pas pris en compte.

## Voir également :
- MDN Web Docs Date Reference: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date
- Luxon Documentation: https://moment.github.io/luxon/#/
- date-fns Documentation: https://date-fns.org/

Ces liens sont des ressources utiles pour approfondir les détails de la manipulation de dates en JavaScript.
