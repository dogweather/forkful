---
title:    "Javascript: Obtenir la date actuelle."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi 

La fonction « Date » est un outil crucial pour toute personne travaillant avec JavaScript. Il permet de récupérer et de manipuler facilement la date et l'heure actuelles dans vos programmes. Que vous travailliez sur une application Web ou sur un projet JavaScript, il est essentiel de pouvoir accéder à la date et à l'heure actuelles dans votre code.

## Comment faire 

Pour récupérer la date et l'heure actuelles dans votre code JavaScript, vous pouvez utiliser la méthode "new Date()". Cela créera un objet Date avec la date et l'heure actuelles par défaut. Vous pouvez également passer des paramètres à cette méthode pour obtenir une date spécifique. Par exemple :

```Javascript
let date = new Date(); // Date actuelle
console.log(date); // Sortie : Fri Jan 01 2021 12:00:00 GMT+0100 (heure normale d’Europe centrale)

let date = new Date(2020, 11, 25); // Date spécifique (décembre 25, 2020)
console.log(date); // Sortie : Fri Dec 25 2020 00:00:00 GMT+0100 (heure normale d’Europe centrale)
```

En utilisant la méthode "get" sur l'objet Date, vous pouvez récupérer des informations spécifiques telles que le jour, le mois, l'année, l'heure, les minutes, etc. Par exemple :

```Javascript
let date = new Date();
console.log(date.getFullYear()); // Sortie : 2021 (année actuelle)
console.log(date.getMonth()); // Sortie : 0 (le mois est indexé à partir de 0)
```

Vous pouvez également utiliser la méthode "toLocaleString()" pour obtenir la date et l'heure dans un format spécifique à la langue et à la région de l'utilisateur. Par exemple :

```Javascript
let date = new Date();
console.log(date.toLocaleString('fr-FR')); // Sortie : 01/01/2021 à 12:00:00 (format français)
```

## Plongée plus profonde 

La méthode "Date" possède une multitude de fonctionnalités et de paramètres que vous pouvez explorer pour obtenir la date et l'heure exactes dont vous avez besoin. Vous pouvez également utiliser des librairies tierces comme Moment.js pour faciliter la manipulation de dates dans votre code.

N'oubliez pas également que la date et l'heure sont souvent affectées par le fuseau horaire de l'utilisateur, il est donc important d'en tenir compte lors de la mise en œuvre de ces fonctionnalités dans votre code.

## Voir aussi 

- [Documentation JavaScript Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Guide de Moment.js](https://momentjs.com/docs/)
- [Fuseau horaire en JavaScript](https://www.w3schools.com/jsref/jsref_gettimezoneoffset.asp)

Merci d'avoir lu cet article sur la récupération de la date actuelle en JavaScript. Nous espérons que cela vous aidera dans vos projets de codage en JavaScript ! N'hésitez pas à explorer les différentes fonctionnalités de la méthode "Date" pour répondre à vos besoins spécifiques. À la prochaine fois !