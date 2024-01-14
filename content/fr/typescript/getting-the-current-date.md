---
title:    "TypeScript: Obtenir la date actuelle"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle est une tâche courante en programmation, que ce soit pour afficher la date dans une application ou pour effectuer des calculs basés sur la date. Heureusement, TypeScript dispose de fonctionnalités intégrées pour faciliter cette tâche.

## Comment faire

Pour obtenir la date actuelle en TypeScript, vous pouvez utiliser la méthode `Date.now()` qui renvoie le nombre de millisecondes écoulées depuis le 1er janvier 1970 à 00:00:00 UTC. Cela peut sembler confuse, mais il s'agit en fait d'une méthode utile car elle permet de stocker la date en tant que nombre, ce qui facilite la manipulation et les calculs.

```TypeScript
let currentDate = Date.now();

console.log(currentDate);
// 1625590474264
```

Pour afficher la date dans un format facile à lire, vous pouvez utiliser la fonction `new Date()` qui prend en paramètre une chaîne de caractères représentant une date valide. Vous pouvez ensuite utiliser les méthodes `getFullYear()`, `getMonth()`, `getDate()` et `getDay()` pour obtenir respectivement l'année, le mois, le jour et le jour de la semaine.

```TypeScript
let today = new Date();

console.log(today); // Tue Jul 06 2021 16:01:14 GMT+0200 (heure d’été d’Europe centrale)
console.log(today.getFullYear()); // 2021
console.log(today.getMonth()); // 6 (Note: les mois commencent à 0, donc 6 correspond à juillet)
console.log(today.getDate()); // 6
console.log(today.getDay()); // 2 (Note: les jours commencent à 0, donc 2 correspond à mardi)
```

## Plongée en profondeur

Si vous souhaitez utiliser une date différente de la date actuelle, vous pouvez passer des paramètres à la fonction `Date()` sous la forme `Date(annee, mois, jour)`. Notez que pour le mois, le numérotage commence à 0, donc 0 correspond à janvier et 11 correspond à décembre.

```TypeScript
let christmas = new Date(2021, 11, 25);

console.log(christmas); // Sat Dec 25 2021 00:00:00 GMT+0100 (heure normale d’Europe centrale)
```

Vous pouvez également effectuer des opérations sur les dates en utilisant les méthodes `setFullYear()`, `setMonth()`, `setDate()` et `setDay()`.

```TypeScript
let futureDate = new Date();

futureDate.setFullYear(2022);
console.log(futureDate); // Sun Jul 06 2022 16:09:50 GMT+0200 (heure d’été d’Europe centrale)

futureDate.setMonth(11);
console.log(futureDate); // Mon Dec 06 2021 16:09:50 GMT+0200 (heure d’été d’Europe centrale)

futureDate.setDate(25);
console.log(futureDate); // Sun Dec 25 2022 16:09:50 GMT+0100 (heure normale d’Europe centrale)

futureDate.setDay(0);
console.log(futureDate); // Sun Dec 26 2021 16:09:50 GMT+0100 (heure normale d’Europe centrale)
```

## Voir aussi
- [Documentation officielle TypeScript sur les dates](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#new-date)
- [Article Medium sur la manipulation de dates en TypeScript](https://medium.com/@chandrapraban