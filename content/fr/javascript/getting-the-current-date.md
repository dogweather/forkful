---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:13.410826-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Récupérer la date actuelle en JavaScript, c'est obtenir le jour exact où on se trouve — date, mois, année, et plus. Ça sert à enregistrer des événements, faire des chronomètres, afficher des décomptes, etc.

## How to:
Pour attraper la date d'aujourd'hui:
```Javascript
let today = new Date();
console.log(today);
```
Sortie possible:
```
2023-04-02T12:00:00.000Z
```

Pour afficher jolie:
```Javascript
console.log(today.toLocaleDateString());
```
Sortie exemple:
```
02/04/2023
```

## Deep Dive
JavaScript utilise l'objet `Date` qui date des débuts du langage. C'est l'outil de base pour manipuler dates et heures. Il y a des alternatives comme `Moment.js`, mais c'est souvent plus que nécessaire. L'objet `Date` est construit sur le temps universel coordonné (UTC) et les fuseaux horaires sont gérés avec des méthodes comme `getUTC...()` ou le réglage de `getTimezoneOffset()`.

## See Also
- Documentation MDN sur `Date`: [MDN Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Libraries de date alternatives: [Date-fns](https://date-fns.org/), [day.js](https://day.js.org/)
- Formatage de dates: [Intl.DateTimeFormat](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)
