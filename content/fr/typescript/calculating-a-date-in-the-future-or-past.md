---
title:                "TypeScript: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile dans de nombreuses situations en programmation. Cela permet de planifier des tâches ou des événements à venir ou de manipuler des données temporelles dans une application.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en TypeScript, il existe plusieurs méthodes. La plus simple est d'utiliser la classe `Date` et ses méthodes `getDate()` et `setDate()` pour incrémenter ou décrémenter une date en fonction de vos besoins.

```TypeScript
// Définir la date actuelle
let date = new Date();

// Calculer la date dans 7 jours
date.setDate(date.getDate() + 7);
console.log(date); // Output: Sat Jul 24 2021 15:00:00 GMT+0200 (Central European Summer Time)

// Calculer la date dans 1 an
date.setFullYear(date.getFullYear() + 1);
console.log(date); // Output: Sun Jul 24 2022 15:00:00 GMT+0200 (Central European Summer Time)
```

Vous pouvez également utiliser des bibliothèques tierces comme Moment.js qui offre des fonctionnalités plus avancées pour la manipulation des dates.

## Plongée profonde

Il est important de prendre en compte les différents fuseaux horaires lors de la manipulation de dates. En TypeScript, vous pouvez utiliser la classe `Intl.DateTimeFormat` pour afficher les dates dans le fuseau horaire de votre choix.

```TypeScript
// Définir la date actuelle
let date = new Date();

// Afficher la date en utilisant le fuseau horaire UTC
const options = { timeZone: "UTC" };
const formatter = new Intl.DateTimeFormat("fr-FR", options);
console.log(formatter.format(date)); // Output: 2021-07-17T13:00:00.000Z
```

Il est également important de manipuler les dates avec précision, en prenant en compte les années bissextiles et les différents nombres de jours dans chaque mois. Pour cela, vous pouvez utiliser la bibliothèque date-fns qui offre des fonctions de manipulation des dates plus précises.

## Voir aussi

- [Manipulating Dates in TypeScript](https://devblogs.microsoft.com/typescript/manipulating-dates-in-typescript/)
- [Handling Time and Date in TypeScript Applications](https://blog.greenroots.info/handling-time-&-date-in-typescript-applications)
- [Moment.js Documentation](https://momentjs.com/docs/)
- [date-fns Documentation](https://date-fns.org/docs/)