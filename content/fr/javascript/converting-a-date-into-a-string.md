---
title:    "Javascript: Convertir une date en chaîne de caractères"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Javascript, il est courant de devoir manipuler des dates. Cependant, les dates sont souvent stockées dans un format spécifique et peuvent être délicates à manipuler. C'est pourquoi il est utile de savoir comment convertir une date en chaîne de caractères. Cela permettra de faciliter la manipulation et l'affichage de dates dans votre code.

## Comment faire

Pour convertir une date en chaîne de caractères en Javascript, il existe plusieurs méthodes que l'on peut utiliser en fonction du format de date souhaité. Voici des exemples de code pour convertir une date en différents formats.

```Javascript
// Récupération de la date actuelle
const date = new Date();

// Convertir en chaîne de caractères au format "jj/mm/aaaa"
const dateString1 = date.getDate() + "/" + (date.getMonth() + 1) + "/" + date.getFullYear();
console.log(dateString1); // Output: 31/12/2021

// Convertir en chaîne de caractères au format "aaaa-mm-jj"
const dateString2 = date.toISOString().substring(0, 10);
console.log(dateString2); // Output: 2021-12-31 
```

Ces exemples utilisent les méthodes `getDate()`, `getMonth()`, `getFullYear()` et `toISOString()` pour récupérer les différentes parties de la date et les convertir en chaîne de caractères. Vous pouvez également utiliser des librairies telles que Moment.js pour faciliter la manipulation de dates en Javascript.

## Plongez plus en profondeur

Lors de la conversion d'une date en chaîne de caractères, il est important de comprendre que la représentation de la date peut varier en fonction de la localisation de l'utilisateur. Cela signifie que les méthodes telles que `getDate()` et `getMonth()` peuvent retourner des valeurs différentes dans un environnement où la langue et le fuseau horaire sont différents de celui de l'utilisateur.

Il est également important de prendre en compte le fait qu'une fois une date convertie en chaîne de caractères, il peut être plus difficile de la manipuler et de la comparer avec d'autres dates. Cela peut causer des problèmes lors de la création de conditions pour des opérations telles que la vérification de la validité d'une date.

Il est donc essentiel de bien comprendre les méthodes et les formats de date avant de les utiliser dans votre code.

## Voir aussi

- [Guide de référence Javascript pour travailler avec les dates](https://www.w3schools.com/js/js_dates.asp)
- [Documentation officielle de Moment.js](https://momentjs.com/docs/)
- [Convertir une date en chaîne de caractères en utilisant Intl.DateTimeFormat](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Intl/DateTimeFormat)