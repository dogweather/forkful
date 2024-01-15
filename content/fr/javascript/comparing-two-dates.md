---
title:                "Comparaison de deux dates"
html_title:           "Javascript: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est un outil utile pour de nombreuses applications de programmation telles que la gestion d'événements ou la recherche de données dans une base de données selon une plage de temps spécifique.

## Comment Faire

Pour comparer deux dates en Javascript, vous pouvez utiliser la méthode `getTime()` qui renvoie le nombre de millisecondes écoulées depuis le 1er janvier 1970. Voici un exemple de code montrant comment utiliser cette méthode pour comparer deux dates :

```Javascript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-01-07");

if (date1.getTime() > date2.getTime()) {
  console.log("Date1 est postérieure à Date2");
} else if (date1.getTime() < date2.getTime()) {
  console.log("Date1 est antérieure à Date2");
} else {
  console.log("Date1 est égale à Date2");
}
```

Lors de l'exécution de ce code, l'output sera "Date1 est antérieure à Date2". Cela signifie que Date1 est plus tôt que Date2.

## Plongée en Profondeur

En plus de la méthode `getTime()`, Javascript offre également d'autres méthodes pour comparer des dates telles que `getDate()`, `getMonth()`, `getFullYear()` et `getDay()`. Il est important de noter que ces méthodes renvoient des valeurs basées sur le fuseau horaire local de l'utilisateur. Il est donc essentiel de prendre en compte le fuseau horaire lors de la comparaison de dates.

Un autre point à prendre en considération est que les années bissextiles peuvent affecter la comparaison des dates en fonction du nombre de jours dans le mois de février. Pour éviter cela, il est recommandé d'utiliser une bibliothèque de gestion des dates telle que moment.js.

## A Voir Aussi

- [Documentation Javascript sur les objets Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Documentation moment.js sur la gestion des dates en Javascript](https://momentjs.com/docs/)