---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "Javascript: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Parser une date à partir d'une chaîne de caractères est une opération couramment utilisée en programmation pour convertir une date écrite en une représentation interne que le code peut manipuler. Cela est utile pour traiter les données de différentes sources ou pour afficher des dates dans un format spécifique.

## Comment procéder:

```Javascript
// Exemple 1: Utilisation de la méthode parse() de l'objet Date
const dateString = "01/01/2020";
const date = new Date(Date.parse(dateString));
console.log(date); // Affiche "Wed Jan 01 2020 00:00:00 GMT+0100 (Central European Standard Time)"

// Exemple 2: Utilisation de bibliothèques telles que Moment.js
const parsedDate = moment(dateString, "DD/MM/YYYY").toDate();
console.log(parsedDate); // Affiche "Wed Jan 01 2020 00:00:00 GMT+0100 (Central European Standard Time)"
```

## Plongée en profondeur:

La nécessité de parser une date à partir d'une chaîne de caractères a sa source dans l'utilisation de différentes normes pour représenter les dates, telles que le format UTC ou le format ISO 8601. En plus des méthodes natives de l'objet Date telles que `Date.parse()`, il existe des bibliothèques spécialisées telles que Moment.js qui offrent une syntaxe plus simple et plus flexible pour travailler avec des dates.

## Voir aussi:

- [Documentation de l'objet Date sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Référence/Objets_globaux/Date)
- [Documentation de Moment.js](https://momentjs.com/)