---
title:    "TypeScript: Comparer deux dates"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est la comparaison de deux dates. Cela peut être utile pour déterminer si une date est antérieure ou postérieure à une autre, ou encore pour calculer la différence entre les deux.

## Comment faire

Il existe plusieurs façons de comparer des dates en TypeScript. Voici quelques exemples de code avec leur sortie correspondante :

```TypeScript
//Création de deux dates
const date1 = new Date(2020, 9, 1);
const date2 = new Date(2020, 9, 5);

//Comparaison de dates simples avec "<" et ">"
if (date1 < date2) {
  console.log("La première date est antérieure à la seconde.");
} else {
  console.log("La première date est postérieure à la seconde.");
}

//Comparaison de dates avec ".getTime()" qui renvoie un timestamp en millisecondes
if (date1.getTime() < date2.getTime()) {
  console.log("La première date est antérieure à la seconde.");
} else {
  console.log("La première date est postérieure à la seconde.");
}

//Calcul de la différence de jours entre les deux dates
const difference = (date2.getTime() - date1.getTime()) / (1000 * 60 * 60 * 24);
console.log("Il y a " + difference + " jours entre les deux dates.");
```

Sortie :

```
La première date est antérieure à la seconde.
La première date est antérieure à la seconde.
Il y a 4 jours entre les deux dates.
```

## Plongée en profondeur

En plus de ces méthodes de comparaison, il existe d'autres fonctions utiles pour travailler avec des dates en TypeScript. En voici quelques-unes :

- `.getMonth()` : renvoie le mois de la date (0 pour janvier, 11 pour décembre)
- `.getDay()` : renvoie le jour de la semaine (0 pour dimanche, 6 pour samedi)
- `.getFullYear()` : renvoie l'année de la date
- `.setDate()` : définit le jour de la date en spécifiant une valeur numérique
- `.toLocaleDateString()` : renvoie une représentation de la date sous forme de chaîne de caractères selon la locale du navigateur

Il est également possible de comparer des dates en utilisant des bibliothèques telles que "date-fns" ou "moment.js". Ces bibliothèques offrent des fonctions avancées pour travailler avec des dates et permettent de comparer des dates avec des critères plus précis (comme uniquement le jour ou le mois).

## Voir aussi

- [Documentation sur les dates en TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#date-allows-a-date-type)
- [Comparaison de dates en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Dates)
- [Bibliothèque date-fns pour TypeScript](https://date-fns.org/docs/Getting-Started)