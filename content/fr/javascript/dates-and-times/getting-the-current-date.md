---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:47.977989-07:00
description: "Obtenir la date courante en JavaScript est une t\xE2che fondamentale,\
  \ qui implique de r\xE9cup\xE9rer et \xE9ventuellement de manipuler la date et l'heure\
  \ du jour.\u2026"
lastmod: '2024-03-13T22:44:58.287078-06:00'
model: gpt-4-0125-preview
summary: "Obtenir la date courante en JavaScript est une t\xE2che fondamentale, qui\
  \ implique de r\xE9cup\xE9rer et \xE9ventuellement de manipuler la date et l'heure\
  \ du jour."
title: Obtenir la date actuelle
weight: 29
---

## Quoi et Pourquoi ?
Obtenir la date courante en JavaScript est une tâche fondamentale, qui implique de récupérer et éventuellement de manipuler la date et l'heure du jour. Les programmeurs réalisent cette opération pour afficher les dates sur les sites web, dans les applications, pour suivre les interactions des utilisateurs ou pour gérer des données sensibles au temps.

## Comment faire :
En JavaScript de base, l'objet `Date` est utilisé pour travailler avec les dates et les heures. Voici comment vous pouvez obtenir la date et l'heure actuelles :

```javascript
const currentDate = new Date();
console.log(currentDate); // Exemple de sortie : Ven 14 Avr 2023 12:34:56 GMT+0100 (Heure d'été britannique)
```

Pour afficher uniquement la date dans un format plus convivial, vous pouvez utiliser des méthodes comme `toLocaleDateString()` :

```javascript
console.log(currentDate.toLocaleDateString()); // Exemple de sortie : 14/04/2023
```

Pour avoir plus de contrôle sur le format, des bibliothèques tierces comme *Moment.js* ou *date-fns* sont très populaires, bien qu'il soit bon de savoir que Moment.js est maintenant considéré comme un projet hérité en mode maintenance.

En utilisant *Moment.js* :

```javascript
const moment = require('moment'); // en supposant Node.js ou l'utilisation d'un module bundler
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Exemple de sortie : 2023-04-14
```

Avec *date-fns*, qui met l'accent sur la modularisation en vous permettant d'importer uniquement ce dont vous avez besoin :

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Exemple de sortie : 2023-04-14
```

Chaque approche offre différents niveaux de commodité et de flexibilité pour travailler avec les dates en JavaScript, depuis l'objet `Date` intégré jusqu'aux capacités de formatage et de manipulation plus sophistiquées disponibles à travers les bibliothèques.
