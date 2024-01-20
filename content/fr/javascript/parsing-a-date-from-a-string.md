---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:36:57.775869-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Parsez une date depuis une chaîne de caractères, c’est-à-dire transformer du texte en une date utilisable en JavaScript. Les développeurs le font pour traiter les entrées utilisateur, interagir avec des bases de données, ou simplement pour manipuler des formats de date divers.

## Comment faire :
```javascript
// Utiliser l'objet Date pour le parsing
const dateString = "2023-04-05T14:30:00.000Z";
const dateObject = new Date(dateString);

console.log(dateObject);
// Affiche Wed Apr 05 2023 16:30:00 GMT+0200 (heure d’été d’Europe centrale)

// Une autre option plus robuste avec date-fns
// npm install date-fns
import { parseISO } from 'date-fns';

const date = parseISO(dateString);

console.log(date);
// Affiche Wed Apr 05 2023 16:30:00 GMT+0200 (heure d’été d’Europe centrale)
```

## Deep Dive
Historiquement, parsez des dates en JS était pénible à cause des différences entre navigateurs. L’objet `Date` natif de JS est limité. Des librairies telles que Moment.js (de moins en moins utilisée car volumineuse) et date-fns sont apparues pour y remédier. `date-fns` offre des fonctions légères et consistantes comme `parseISO`, idéale pour les formats ISO 8601. Côté implémentation, prudence avec les fuseaux horaires et formats du monde entier; déléguer à des librairies éprouvées est souvent la meilleure approche.

## Voir aussi :
- Documentation MDN sur `Date`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date
- Date-fns: https://date-fns.org/
- Une comparaison des librairies de dates: https://www.smashingmagazine.com/2020/07/comparison-popular-datetime-libraries-dates/