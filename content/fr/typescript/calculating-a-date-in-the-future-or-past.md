---
title:    "TypeScript: Calcul d'une date future ou passée"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile pour planifier des événements, gérer des données temporaires ou créer des rappels. Les programmes ayant besoin d'une compréhension du temps peuvent bénéficier de cette fonctionnalité.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en TypeScript, il suffit d'utiliser la classe `Date` de JavaScript avec des opérations mathématiques pour ajouter ou soustraire une période de temps.

```TypeScript
// Calculer une date dans le futur
let date = new Date();
date.setDate(date.getDate() + 7);
console.log(`La date dans une semaine sera: ${date.toDateString()}`);

// Calculer une date dans le passé
date = new Date();
date.setFullYear(date.getFullYear() - 2);
console.log(`La date d'il y a deux ans était: ${date.toDateString()}`);
```
### Sortie:

> La date dans une semaine sera: Sun Oct 03 2021
>
> La date d'il y a deux ans était: Fri Oct 02 2019

En utilisant `setDate()` et `setFullYear()` avec les opérateurs d'addition ou de soustraction, il est possible de calculer une date dans le futur ou dans le passé. Il est également possible de manipuler d'autres unités de temps telles que les heures, les minutes et les secondes en utilisant les méthodes `setHours()`, `setMinutes()` et `setSeconds()`.

## Plongée en profondeur

En plus de la classe `Date`, TypeScript offre également des fonctionnalités de types stricts pour gérer les dates et les temps. Cela peut éviter les erreurs courantes telles que l'ajout ou la soustraction de nombres à une date.

Par exemple, si l'on essaie d'utiliser la méthode `setDate()` avec un nombre négatif, TypeScript renverra une erreur de compilation en indiquant que ce n'est pas une valeur autorisée pour cette méthode.

Il est également possible d'utiliser des bibliothèques externes telles que `moment.js` pour des fonctionnalités plus avancées de manipulation des dates et des temps.

## Voir aussi

- [Documentation de la classe Date en TypeScript](https://www.typescriptlang.org/docs/handbook/classes.html#instance-side)
- [Documentation de la bibliothèque moment.js](https://momentjs.com/docs/)