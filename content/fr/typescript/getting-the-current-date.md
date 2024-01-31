---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:16:46.582308-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Obtenir la date courante, c'est récupérer la date et l'heure actuelles de l'ordinateur où s'exécute le programme. Les développeurs font ça pour des tâches comme horodater des événements, des logs, ou pour manipuler des données temporelles.

## Comment faire :

```typescript
const maintenant: Date = new Date();
console.log(maintenant);
```

Sortie (exemple) :
```
2023-03-25T14:45:30.655Z
```

## Plongée en profondeur

La classe `Date` en JavaScript, que TypeScript étend, date des premières versions de ECMAScript. C'est la méthode standard pour gérer les dates heure. Il y a d'autres bibliothèques comme `moment.js` ou `date-fns` qui offrent plus de fonctionnalités et une meilleure gestion des zones horaires, mais pour les bases, `Date` est suffisant.

`Date` utilise l'heure UTC (Temps Universel Coordonné) pour créer des dates, mais vous pouvez manipuler des données selon le fuseau horaire local avec des méthodes telles que `toLocaleDateString()` ou `toLocaleTimeString()`.

Exemple de ces méthodes:

```typescript
console.log(maintenant.toLocaleDateString()); // '25/03/2023' en fonction de la localisation
console.log(maintenant.toLocaleTimeString()); // '14:45:30' en fonction de la localisation
```

Il est également important de se rappeler que le temps est mesuré en millisecondes depuis l'époque UNIX, qui est le 1er janvier 1970. Cette mesure est ce qui permet de calculer les intervalles de temps et de comparer des dates.

## Voir aussi

- MDN Web Docs sur l'objet Date : [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js, une bibliothèque populaire pour gérer les dates : [Moment.js](https://momentjs.com/)
- Date-fns, une bibliothèque moderne pour manipuler les dates : [date-fns](https://date-fns.org/)
- ECMAScript, la spécification sur laquelle se base JavaScript : [ECMAScript](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)
