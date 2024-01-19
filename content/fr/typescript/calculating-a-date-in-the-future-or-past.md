---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "TypeScript: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcul de dates futures et passées en TypeScript

## Qu'est-ce que c'est et pourquoi ?

Le calcul de dates futures ou passées consiste à déterminer un point précis dans le temps à partir d'une date de référence. En tant que programmeurs, nous faisons cela pour résoudre des problèmes complexes liés à la planification, aux rappels, aux échéances, etc.

## Comment faire ?

Avec TypeScript, nous utilisons l'objet `Date` et ses méthodes pour le faire. Voici un exemple simple pour calculer une date future :

```TypeScript
let date = new Date();
date.setDate(date.getDate() + 7); // Ajoute 7 jours à la date actuelle
console.log(date);
```

Pour calculer une date passée :

```TypeScript
let date = new Date();
date.setDate(date.getDate() - 7);  // Soustrait 7 jours de la date actuelle
console.log(date);
```

Dans ces exemples, `getDate()` retourne le jour du mois de l'objet `Date`, et `setDate()` définit le jour du mois de l'objet `Date`.

## Plongée en profondeur 

Historiquement, le calcul de dates en programmation a été une tâche ardue en raison des variations de calendriers, des fuseaux horaires et des anomalies comme l'année bissextile. TypeScript a simplifié cette tâche avec l'objet `Date` et ses méthodes. 

Comme alternative à l'approche ci-dessus, vous pouvez également utiliser une bibliothèque de gestion des dates comme `moment.js` pour traiter ces calculs de manière plus expressive et concise.

En matière d'implémentation, l'objet `Date` en TypeScript (et JavaScript) stocke les dates en millisecondes depuis la référence Unix Epoch (1er janvier 1970). C'est pourquoi nous pouvons ajouter ou soustraire des jours simplement en utilisant les millisecondes.

## Voir aussi 

- Documentation de TypeScript `Date`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date
- Bibliothèque `moment.js`: https://momentjs.com/
- Comment JavaScript travaille avec les dates et le temps : https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript