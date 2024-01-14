---
title:                "Javascript: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
La conversion d'une date en chaîne de caractères est une compétence utile à avoir lors de la programmation en Javascript. Cela peut être nécessaire lors de l'affichage de dates sur un site web ou lors de l'interaction avec des bases de données.

## Comment faire
Pour convertir une date en chaîne de caractères en Javascript, il existe une méthode appelée `toString()` qui peut être appliquée à un objet Date. Voici un exemple de code :

```Javascript
let today = new Date();
let dateAsString = today.toString();
```

Lorsque ce code est exécuté, la variable `dateAsString` contiendra la date actuelle sous forme de chaîne de caractères. Selon l'heure et le fuseau horaire de votre ordinateur, la sortie peut ressembler à ceci : "Thu Jun 10 2021 13:30:00 GMT+0200 (Central European Summer Time)".

Vous pouvez également spécifier le format de la chaîne de caractères en utilisant les méthodes `getDate()`, `getMonth()` et `getFullYear()` pour obtenir les parties de la date que vous souhaitez inclure.

```Javascript
let today = new Date();
let day = today.getDate();
let month = today.getMonth() + 1;
let year = today.getFullYear();
let dateAsString = day + "/" + month + "/" + year;
```

La variable `dateAsString` contiendra maintenant la date actuelle sous forme de "jj/mm/aaaa".

## Plongée en profondeur
Lorsque vous utilisez la méthode `toString()` pour convertir une date en chaîne de caractères, il est important de noter que la sortie peut varier en fonction du navigateur et du système d'exploitation utilisés. Pour une sortie plus cohérente, il est conseillé d'utiliser une bibliothèque de gestion de dates telle que Moment.js.

De plus, la méthode `toString()` peut également inclure des informations sur le fuseau horaire, ce qui peut être déroutant lors de l'affichage des dates à destination des utilisateurs. Il est donc important de bien comprendre et de manipuler les dates avant de les convertir en chaînes de caractères.

## Voir aussi
- [Documentation JavaScript sur la méthode toString()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/toString)
- [Documentation Moment.js](https://momentjs.com/)
- [Article sur la manipulation des dates en JavaScript](https://zellwk.com/blog/js-dates/)