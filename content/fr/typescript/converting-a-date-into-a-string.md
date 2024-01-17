---
title:                "Convertir une date en chaîne de caractères"
html_title:           "TypeScript: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?

Convertir une date en chaîne de caractères est essentiellement le fait de prendre une date ou une heure et de la formater de manière lisible pour les humains. Les programmeurs le font pour afficher ou stocker des dates dans un format compréhensible pour tout le monde.

## Comment:

```TypeScript
// Convertir une date en chaîne de caractères au format DD/MM/YYYY
let date = new Date("2021-06-01");
let dateString = date.toLocaleDateString("fr-FR");
console.log(dateString); // Affiche "01/06/2021"

// Convertir une heure en chaîne de caractères au format 24 heures
let time = new Date("2021-06-01 15:30:00");
let timeString = time.getHours() + ":" + time.getMinutes();
console.log(timeString); // Affiche "15:30"

// Convertir une date et une heure en chaîne de caractères personnalisée
let dateTime = new Date("2021-06-01 15:30:00");
let options = { weekday: 'long', year: 'numeric', month: 'short', day: 'numeric' };
let dateTimeString = dateTime.toLocaleDateString("fr-FR", options);
console.log(dateTimeString); // Affiche "mardi 01 juin 2021"

```

## Plongée en profondeur:

Avant l'avènement des ordinateurs, les dates étaient souvent écrites sous forme de nombres, ce qui les rendait difficiles à lire et à comprendre pour les humains. En convertissant une date en chaîne de caractères, les programmeurs sont en mesure de présenter la date de manière plus lisible, tout en choisissant un format qui convient à leur public. D'autres alternatives sont d'utiliser des librairies JavaScript telles que Moment.js pour faciliter la manipulation des dates, ou d'utiliser une base de données qui prend en charge les champs de type date.

## Voir aussi:

- Documentation officielle de TypeScript sur les dates: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#improved-regex-types
- Moment.js - Librairie JavaScript pour manipuler, valider et formater les dates: https://momentjs.com/
- Stack Overflow - Discussions et solutions concernant la conversion de dates en chaînes de caractères en TypeScript: https://stackoverflow.com/questions/55180305/how-to-convert-typedate-to-string-in-angular-7