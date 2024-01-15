---
title:                "Obtenir la date actuelle"
html_title:           "Javascript: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est important de connaître la date actuelle en programmation JavaScript. Eh bien, la réponse est simple : la date est un élément essentiel dans la plupart des applications, que ce soit pour afficher dynamiquement l'heure et la date sur un site web ou pour effectuer des opérations temporaires dans une application.

## Comment Faire

Il existe plusieurs façons de récupérer la date actuelle en JavaScript, mais voici la méthode la plus couramment utilisée :

```javascript
let currentDate = new Date();
console.log(currentDate);
```

Cette méthode utilise l'objet `Date()` pour créer une instance de la date actuelle et la stocke dans la variable `currentDate`. La méthode `console.log()` permet ensuite d'afficher cette date dans la console du navigateur.

Il est également possible de formater la date en utilisant différentes méthodes telles que `getFullYear()`, `getMonth()`, `getDate()` et `getDay()`, qui renvoient respectivement l'année, le mois, le jour du mois et le jour de la semaine de la date actuelle. Par exemple :

```javascript
let year = currentDate.getFullYear();
let month = currentDate.getMonth() + 1; // Les mois commencent à partir de 0, donc on ajoute 1 pour avoir le mois actuel
console.log(`Nous sommes en ${month}/${year}.`); // Output: Nous sommes en 8/2021.
```

## Plongée Profonde

Maintenant que vous savez comment récupérer la date actuelle, il est important de comprendre que cette date est stockée en tant que nombre en millisecondes depuis le 1er janvier 1970. C'est ce qu'on appelle le "temps Unix" et cela facilite les calculs de dates et d'heures.

Vous pouvez également modifier la date en utilisant des méthodes telles que `setFullYear()`, `setMonth()`, `setDate()` et `setTime()`. Par exemple :

```javascript
let newDate = new Date();
newDate.setFullYear(2022);
console.log(newDate); // Output: Sat Jan 01 2022 10:56:14 GMT+0100 (heure normale d’Europe centrale)
```

Enfin, il est important de noter que la date et l'heure peuvent varier en fonction du fuseau horaire de l'utilisateur. Pour obtenir l'heure locale, utilisez la méthode `toLocaleString()`. Par exemple :

```javascript
let localTime = currentDate.toLocaleString();
console.log(localTime); // Output: 19/08/2021 à 10:56:14
```

## Voir Aussi

Si vous souhaitez en savoir plus sur la manipulation des dates en JavaScript, voici quelques ressources utiles :

- [Documentation officielle de JavaScript sur l'objet Date()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Gérer les dates en JavaScript comme un pro](https://www.lifewire.com/using-the-date-object-to-manipulate-dates-and-times-2038093)
- [Manipuler les dates et les heures avec moment.js](https://momentjs.com/)