---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La conversion d'une date sous forme de chaîne en un objet Date en Javascript est un processus appelé analyse de date (date parsing). Cela est nécessaire pour manipuler facilement les dates et les heures dans votre code. 

## Comment faire :

Voici un exemple de comment convertir une chaîne en date en Javascript :

```Javascript
let dateStr = "2022-04-20T14:27:00.000Z";
let dateObj = new Date(dateStr);
console.log(dateObj);
```

Dans cet exemple, l'objet Date est créé avec la chaîne ISO8601.
L'output ressemblerait à ceci :

```Javascript
Wed Apr 20 2022 16:27:00 GMT+0200 (heure d’été d’Europe centrale)
```

## Deep Dive :

1. Historique : La capacité de Javascript à analyser des dates à partir de chaînes a été introduite pour la première fois avec ECMAScript 5 et a été perfectionnée avec des fonctions additionnelles dans ECMAScript 2015 (6th Edition).
2. Alternatives : Vous pouvez utiliser des bibliothèques telles que Moment.js, Date-fns ou Luxon pour augmenter la facilité et la précision de l'analyse de date.
3. Détails d'implémentation : L'analyse de date en Javascript peut varier en fonction de la chaîne fournie. Par exemple, si vous fournissez une date dans le format "mm/dd/yyyy" en utilisant le new Date(), cela peut provoquer des problèmes de compatibilité entre les navigateurs.

## Voir Aussi :

1. [Date - JavaScript | MDN Web Docs](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Parsing and Formatting Dates | Moment.js](https://momentjs.com/docs/#/parsing/)
3. [Parse | Date-fns](https://date-fns.org/v2.22.1/docs/parse)
4. [Luxon Documentation](https://moment.github.io/luxon/#/)