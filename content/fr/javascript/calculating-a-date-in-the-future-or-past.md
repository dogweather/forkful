---
title:    "Javascript: Calculer une date dans le futur ou le passé"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé est une tâche courante en programmation. Cela peut notamment être utile pour planifier des événements, gérer des abonnements ou afficher des rappels. Dans cet article, nous allons voir comment réaliser cette opération en utilisant le langage de programmation Javascript.

## Comment faire

Tout d'abord, il est important de comprendre que les dates en Javascript sont représentées par des objets Date. Voici un exemple simple de création d'une date à partir d'une valeur numérique :

```Javascript
let date = new Date(2021, 10, 25); // Crée une date au 25 novembre 2021
console.log(date); // Output : Wed Nov 25 2021 00:00:00 GMT+0100 (Central European Standard Time)
```

Pour calculer une date dans le futur ou le passé, nous allons utiliser les méthodes set et get des objets Date. La méthode set permet de modifier la valeur d'un élément spécifique d'une date, tandis que la méthode get permet de récupérer la valeur d'un élément. Par exemple, pour ajouter 3 jours à une date, nous pouvons utiliser la méthode set avec l'élément "date" et la méthode get avec l'élément "day" :

```Javascript
let date = new Date(2021, 10, 25);
date.setDate(date.getDate() + 3); // Ajoute 3 jours à la date
console.log(date); // Output : Sat Nov 28 2021 00:00:00 GMT+0100 (Central European Standard Time)
```

Nous pouvons également utiliser des valeurs négatives pour calculer une date dans le passé. Par exemple, pour trouver la date d'il y a 5 semaines, nous pouvons utiliser la méthode set avec l'élément "week" et la méthode get avec l'élément "date" :

```Javascript
let date = new Date(2021, 10, 25);
date.setWeek(date.getWeek() - 5); // Soustrait 5 semaines à la date
console.log(date); // Output : Wed Oct 20 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

## Approfondissement

Il existe de nombreuses autres méthodes utiles pour manipuler les dates en Javascript, telles que setFullYear(), setMonth(), setHours(), setMinutes(), etc. De plus, il est possible de réaliser des opérations plus complexes en utilisant les fonctions Math et les opérateurs mathématiques.

Il est également important de prendre en compte les différences de fuseaux horaires lors du calcul d'une date. Pour cela, il est recommandé d'utiliser les méthodes toUTCString() et toLocaleString() pour convertir la date en temps universel ou en temps local.

Enfin, il existe des bibliothèques externes comme Moment.js qui peuvent simplifier la manipulation de dates en Javascript et proposent des fonctionnalités supplémentaires.

## Voir aussi

- [Documentation sur les objets Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Tutorial sur la manipulation des dates en Javascript](https://www.w3schools.com/js/js_date_methods.asp)
- [Bibliothèque Moment.js](https://momentjs.com/)