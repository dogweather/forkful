---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "TypeScript: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Calculer une date dans le futur ou dans le passé est une tâche courante pour de nombreux programmeurs. Cela consiste à déterminer une date spécifique en ajoutant ou en soustrayant un certain nombre de jours, de mois ou d'années à une date de référence.

Les programmeurs le font souvent pour gérer des tâches telles que la planification d'événements, la gestion de délais, ou encore pour effectuer des calculs liés aux finances ou à la génération de rapports.

## Comment faire:

Voici quelques exemples de code TypeScript pour calculer une date dans le futur ou dans le passé:

```TypeScript
// Calculer une date dans le futur en ajoutant 10 jours
let date = new Date();
date.setDate(date.getDate() + 10);
console.log(date.toDateString()); // Output: Sun Feb 02 2020

// Calculer une date dans le passé en soustrayant 2 mois
let date = new Date();
date.setMonth(date.getMonth() - 2);
console.log(date.toDateString()); // Output: Mon Nov 02 2020
```

## Plongée en profondeur:

Avant l'invention des ordinateurs, les dates étaient généralement calculées à la main ou à l'aide de calendriers. Avec le développement de la programmation informatique, il est devenu plus facile d'effectuer des calculs de dates de manière précise et rapide.

Il existe également d'autres méthodes pour calculer une date dans le futur ou dans le passé, telles que l'utilisation de bibliothèques externes ou de fonctions de calcul intégrées à d'autres langages de programmation.

En termes d'implémentation, les dates peuvent être représentées de différentes manières selon le langage de programmation utilisé. En TypeScript, les dates sont généralement stockées sous forme de time stamp Unix, c'est-à-dire le nombre de secondes écoulées depuis le 1er janvier 1970.

## Voir aussi:

- Documentation officielle TypeScript pour les dates: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#object-spread-and-rest
- Bibliothèque JavaScript Moment.js pour la manipulation de dates: https://momentjs.com/
- Tutoriel sur les dates en TypeScript: https://www.tutorialspoint.com/typescript/typescript_date.htm