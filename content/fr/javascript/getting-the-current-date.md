---
title:    "Javascript: Obtenir la date actuelle"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date et l'heure actuelles est un concept fondamental dans la programmation. Cela permet aux développeurs de créer des applications dynamiques et de suivre le temps et les événements.

## Comment faire

Pour obtenir la date et l'heure actuelles en Javascript, il existe différentes méthodes. La plus simple est d'utiliser l'objet `Date()` intégré dans le language.

```Javascript
// Créer un nouvel objet Date
let date = new Date();

// Obtenir la date actuelle
let currentDate = date.getDate();

// Obtenir le jour de la semaine (de 0 à 6)
let dayOfWeek = date.getDay();

// Obtenir le mois actuel (de 0 à 11)
let currentMonth = date.getMonth();

// Obtenir l'année actuelle
let currentYear = date.getFullYear();

// Obtenir l'heure actuelle (de 0 à 23)
let currentHour = date.getHours();

// Obtenir les minutes actuelles
let currentMinutes = date.getMinutes();

// Obtenir les secondes actuelles
let currentSeconds = date.getSeconds();

// Obtenir le temps écoulé en millisecondes depuis le 1er janvier 1970
let milliseconds = date.getTime();
```

La méthode `getDate()` renvoie le jour du mois, `getDay()` renvoie le jour de la semaine, `getMonth()` renvoie le mois et `getFullYear()` renvoie l'année. Il existe également des méthodes pour obtenir l'heure, les minutes, les secondes et les millisecondes.

## Plongée en profondeur

L'objet `Date()` offre également d'autres fonctionnalités utiles telles que la manipulation de dates et la comparaison de dates.

Par exemple, pour ajouter un jour à une date donnée, nous pouvons utiliser la méthode `setDate()`.

```Javascript
let date = new Date(2020, 8, 15); // 15 septembre 2020
let newDate = date.setDate(date.getDate() + 1); // 16 septembre 2020
```

De même, pour comparer deux dates, nous pouvons utiliser la méthode `getTime()` qui renvoie le temps en millisecondes et ensuite les comparer.

```Javascript
let date1 = new Date(2020, 8, 15);
let date2 = new Date(2020, 8, 16);
if (date1.getTime() < date2.getTime()) {
  console.log("date1 est avant date2");
}
```

La manipulation et la comparaison de dates peuvent être extrêmement utiles pour les tâches telles que la planification d'événements ou la création d'agenda.

## Voir aussi

- [Documentation sur l'objet Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Exemples de code pour l'utilisation de l'objet Date en Javascript](https://www.w3schools.com/js/js_date_formats.asp)
- [Tutoriel sur la manipulation des dates en Javascript](https://www.tutorialspoint.com/javascript/javascript_date_object.htm)