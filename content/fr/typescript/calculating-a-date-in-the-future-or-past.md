---
title:                "Calculation d'une date dans le futur ou le passé."
html_title:           "TypeScript: Calculation d'une date dans le futur ou le passé."
simple_title:         "Calculation d'une date dans le futur ou le passé."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile pour de nombreuses raisons, notamment pour planifier des événements, gérer des tâches ou même pour des besoins d'analyse de données. Avec TypeScript, cela devient encore plus facile grâce à sa syntaxe claire et son typage fort.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant TypeScript, nous allons utiliser l'objet `Date` intégré dans JavaScript, ainsi que quelques méthodes pratiques de la librairie Moment.js. 

```TypeScript
// Import de Moment.js
import moment from 'moment';

// Calculer une date dans le futur
const futureDate = moment().add(1, 'year');

// Calculer une date dans le passé
const pastDate = moment().subtract(5, 'days');
```

Ces exemples utilisent la méthode `add()` pour ajouter un certain nombre de temps à la date actuelle, et la méthode `subtract ()` pour soustraire un certain nombre de temps. Vous pouvez également utiliser d'autres unités de temps, telles que `months`, `weeks`, `hours`, etc.

Pour obtenir cet exemple en sortie, vous pouvez utiliser `console.log()` pour afficher la date ou utiliser des bibliothèques graphiques pour afficher la date sur une interface utilisateur.

```
Future date: 2022-07-11T04:33:53.073Z
Past date: 2022-07-03T04:33:53.073Z
```

## Deep Dive

En profondeur, JavaScript stocke les dates en tant que nombres de millisecondes depuis le 1er janvier 1970 à 00h00 UTC. Cela signifie que lorsque vous utilisez des objets `Date`, vous travaillez réellement avec des nombres plutôt qu'avec des dates en tant que telles.

La librairie Moment.js est utile car elle simplifie les opérations sur les dates et offre plus de flexibilité dans le formatage et la gestion du temps. Elle a également été conçue pour résoudre les problèmes de compatibilité et de précision rencontrés avec l'objet `Date` en JavaScript.

## Voir aussi

- [Documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
- [Guide pratique de Moment.js](https://momentjs.com/guides/)
- [Calculer des différences de dates avec TypeScript](https://www.educative.io/courses/grokking-the-object-oriented-design-interview/NEOrOwap60n)