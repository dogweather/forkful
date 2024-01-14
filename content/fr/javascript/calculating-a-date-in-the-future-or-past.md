---
title:    "Javascript: Calculer une date dans le futur ou le passé"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile pour de nombreuses raisons en programmation. Par exemple, cela peut être utilisé pour créer des rappels, des horaires, ou pour gérer des délais.

## Comment Faire

Il existe différentes méthodes pour calculer une date dans le futur ou dans le passé en Javascript. Voici quelques exemples :

```Javascript
// Calculer une date dans le futur en utilisant la méthode .getDate()

var today = new Date(); // Crée une nouvelle date contenant la date et l'heure actuelles
var futureDate = new Date(today.getDate() + 7); // Ajoute 7 jours à la date actuelle
console.log(futureDate); // Output: la date dans 7 jours

```

```Javascript
// Calculer une date dans le passé en utilisant la méthode .setDate()

var today = new Date(); // Crée une nouvelle date contenant la date et l'heure actuelles
var pastDate = new Date(today.setDate(today.getDate() - 14)); // Soustrait 14 jours à la date actuelle
console.log(pastDate); // Output: la date il y a 14 jours

```

Il est également possible de spécifier une date précise en utilisant la méthode .setFullYear() ou .setMonth().

## Plongée Profonde

Pour calculer une date dans le futur ou dans le passé, il est important de comprendre comment les dates sont stockées en Javascript. Les dates sont mesurées en millisecondes à partir du 1er janvier 1970, à minuit UTC. Cela signifie que la date actuelle est convertie en millisecondes, puis le nombre de millisecondes correspondant à la date souhaitée est ajouté ou soustrait. De plus, il est important de prendre en compte les différents décalages horaires lors de la manipulation de dates.

Voici quelques ressources pour en savoir plus sur la manipulation de dates en Javascript :

- [Documentation officielle de JavaScript sur les dates](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Article sur les fonctions de manipulation de dates en Javascript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-javascript)
- [Outil de conversion de dates en millisecondes](https://currentmillis.com/)

## Voir Aussi

- [Comment calculer le temps écoulé depuis une date spécifique en Javascript](https://codeburst.io/how-to-calculate-time-elapsed-from-a-specific-date-in-javascript-fs848zfoz)
- [Introduction à la manipulation de dates en Javascript](https://blog.usejournal.com/introduction-to-dates-in-javascript-4dfafaf48ee0) 
- [Les dates et les heures en Javascript - Tuto Javacript](https://www.youtube.com/watch?v=OqGzk9-lMIc)