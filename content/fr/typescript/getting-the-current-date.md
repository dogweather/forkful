---
title:                "Obtenir la date actuelle"
html_title:           "TypeScript: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu’est-ce que c’est et pourquoi?
Obtenir la date actuelle est un moyen courant pour les programmeurs de suivre le temps ou de synchroniser les processus avec l'heure actuelle. Cela peut servir à des fins telles que le suivi des délais, la planification de tâches ou l'affichage de l'heure actuelle dans une application.

## Comment faire:
```
TypeScript const date = new Date ();
console.log (date);
```
Sortie: 
```
Tue Feb 02 2021 12: 30: 00 GMT-0500 (Eastern Standard Time)
```
Vous pouvez également obtenir la date sous forme de chaîne de caractères dans un format spécifique en utilisant les méthodes intégrées de l'objet Date. Par exemple:
```
TypeScript const year = date.getFullYear ();
const month = date.getMonth ();
const day = date.getDate ();
console.log (year + '/' + month + '/' + day);
```
Sortie: 
```
2021/02/02
```

## Deep Dive:
Obtenir la date actuelle peut sembler simple de nos jours, mais cela a en fait nécessité beaucoup plus de travail dans le passé. Auparavant, les programmeurs devaient manipuler manuellement les codes de temps Unix ou utiliser des bibliothèques tierces pour obtenir la date actuelle. Maintenant, avec l'objet Date intégré dans TypeScript, ce processus est devenu beaucoup plus simple et plus pratique.

Bien qu'il existe des alternatives pour l'objet Date, telles que Moment.js et Day.js, qui offrent des fonctionnalités supplémentaires pour la manipulation de dates, l'utilisation de l'objet Date intégré de TypeScript est généralement la méthode privilégiée pour obtenir la date actuelle. De plus, en utilisant les méthodes intégrées de l'objet Date, vous pouvez obtenir la date dans n'importe quel format souhaité.

## Voir aussi:
- [Documentation officielle de l'objet Date en TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Guide pour travailler avec les dates en TypeScript](https://medium.com/javascript-in-plain-english/working-with-dates-in-typescript-2947f14d1103)
- [Comparaison des alternatives pour obtenir la date actuelle en JavaScript](https://stackoverflow.com/questions/10211145/getting-current-date-and-time-in-javascript)