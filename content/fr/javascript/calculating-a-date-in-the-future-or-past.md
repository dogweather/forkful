---
title:                "Javascript: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer des dates dans le futur ou le passé peut être utile dans de nombreuses situations, notamment pour planifier des tâches ou pour afficher des événements à venir sur un calendrier.

## Comment Faire

Pour calculer une date dans le futur ou le passé en utilisant Javascript, il faut utiliser la méthode Date() et ses différentes propriétés et méthodes.

```Javascript
// Pour obtenir la date actuelle
const now = new Date();

// Pour calculer une date dans le futur
const futureDate = new Date(now.getFullYear() + 1, now.getMonth(), now.getDate());

// Pour calculer une date dans le passé
const pastDate = new Date(now.getFullYear() - 1, now.getMonth(), now.getDate());

// Output:
// futureDate: Fri Jan 22 2022 00:00:00 GMT+0100 (Central European Standard Time)
// pastDate: Wed Jan 22 2020 00:00:00 GMT+0100 (Central European Standard Time)
``` 

Il est également possible d'utiliser les méthodes telles que .setFullYear(), .setMonth(), .setDate() pour modifier les valeurs de l'année, du mois ou du jour de la date. Ces méthodes peuvent également être utilisées pour réaliser des calculs plus complexes, comme par exemple ajouter ou soustraire plusieurs années à une date.

```Javascript
// Pour calculer une date dans 3 ans
const dateInThreeYears = new Date();
dateInThreeYears.setFullYear(dateInThreeYears.getFullYear() + 3);

// Output: Tue Jan 22 2024 20:43:55 GMT+0100 (Central European Standard Time)
``` 

## Plongée Profonde

En calculant une date dans le futur ou le passé, il est important de prendre en compte les différences de gestion de temps entre les différents pays et fuseaux horaires. Il est donc recommandé d'utiliser des bibliothèques telles que Moment.js pour une plus grande précision et une meilleure gestion des dates.

De plus, il est important de comprendre le fonctionnement des méthodes Date() et leurs différentes propriétés afin d'éviter les erreurs de calcul.

## Voir Aussi

- [Documentation officielle de Date() en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Moment.js](https://momentjs.com/)
- [Exemples de calculs de dates en Javascript](https://www.w3schools.com/js/js_date_methods.asp)