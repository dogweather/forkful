---
title:                "TypeScript: Calculer une date dans le futur ou le passé"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Le calcul d'une date dans le futur ou le passé peut être très utile pour de nombreux types de programmation, tels que les applications de planification ou les rappels automatiques. Cela vous permet également de personnaliser votre application pour afficher des dates spécifiques à vos besoins.

## Comment faire

```TypeScript
// Calculer une date dans le futur
const currentDate = new Date();
const futureDate = new Date();
futureDate.setDate(currentDate.getDate() + 7); // ajouter 7 jours
console.log(futureDate) // affiche la date dans une semaine à partir de maintenant

// Calculer une date dans le passé
const pastDate = new Date();
pastDate.setDate(currentDate.getDate() - 14); // soustraire 14 jours
console.log(pastDate) // affiche la date il y a deux semaines

// Calculer une date à partir d'une date spécifique
const specificDate = new Date('2021-01-01');
specificDate.setDate(specificDate.getDate() + 30); // ajouter 30 jours
console.log(specificDate); // affiche une date dans un mois à partir du 1er janvier 2021
```

## Plongée en profondeur

Lorsque vous calculez des dates dans le futur ou le passé en TypeScript, il est important de comprendre que le type de données utilisé pour les dates est un objet "Date". Cela signifie que vous pouvez utiliser des méthodes telles que .getDate() pour obtenir le jour d'une date spécifique, .getMonth() pour obtenir le mois et .getFullYear() pour obtenir l'année. Vous pouvez également utiliser les méthodes .setDate(), .setMonth() et .setFullYear() pour modifier ces valeurs.

Une chose importante à retenir est que TypeScript compte les mois à partir de 0, ce qui signifie que le mois de janvier est représenté par 0 et le mois de février par 1. Cela peut causer des erreurs si vous n'êtes pas conscient de cela lors du calcul de dates.

## Voir aussi

- [Documentation officielle de TypeScript sur les objets Date](https://www.typescriptlang.org/docs/handbook/standard-objects.html#date)
- [Calculer des dates dans le futur et le passé en JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Guide complet pour travailler avec des dates en TypeScript](https://felixgerschau.com/typescript-date/)