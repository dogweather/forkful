---
title:                "Javascript: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

Pourquoi: La récupération de la date et de l'heure actuelles est essentielle pour de nombreuses applications Javascript, telles que les applications de réservation ou les outils de suivi du temps.

Comment faire: Il existe plusieurs façons d'obtenir la date et l'heure actuelles en Javascript. Voici quelques-unes des méthodes les plus courantes :

```Javascript
// Méthode 1 : Utiliser la classe Date
let date = new Date();
console.log(date); // Renvoie la date et l'heure actuelles

// Méthode 2 : Utiliser la méthode now()
let date = Date.now();
console.log(date); // Renvoie le nombre de millisecondes depuis le 1er janvier 1970

// Méthode 3 : Utiliser les méthodes getFullYear(), getMonth() et getDate()
let date = new Date();
let annee = date.getFullYear();
let mois = date.getMonth() + 1;
let jour = date.getDate();
console.log(annee + "-" + mois + "-" + jour); // Renvoie la date actuelle au format JJ/MM/AAAA
```

Deep Dive: En creusant davantage, nous pouvons voir que la méthode `new Date()` prend en compte le fuseau horaire de l'utilisateur, tandis que la méthode `Date.now()` renvoie la date et l'heure au fuseau horaire UTC. Les méthodes `getFullYear()`, `getMonth()` et `getDate()` renverront également les informations de date au fuseau horaire local de l'utilisateur. De plus, il est important de comprendre que ces méthodes peuvent varier en fonction du navigateur utilisé.

Voir aussi: Pour plus d'informations sur la gestion du temps en Javascript, consultez les liens suivants :

- https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date
- https://www.w3schools.com/jsref/jsref_obj_date.asp
- https://javascript.info/date-time