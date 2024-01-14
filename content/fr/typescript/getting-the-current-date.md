---
title:                "TypeScript: Obtenir la date actuelle"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle dans un programme est une tâche très courante en programmation TypeScript. Cela peut être utile pour afficher l'heure et la date pour les utilisateurs, enregistrer des données entrées à un certain moment ou pour une variété d'autres fonctionnalités. Dans cet article, nous allons explorer comment utiliser TypeScript pour obtenir la date actuelle.

## Comment ça marche

L'API JavaScript native ```Date``` est largement utilisée pour gérer les dates et heures dans TypeScript. Pour obtenir la date actuelle, nous pouvons créer une nouvelle instance de ```Date``` en utilisant ```new Date()```, comme ci-dessous :

```TypeScript
const currentDate = new Date();
```

La variable ```currentDate``` contiendra maintenant la date et l'heure actuelles. Pour afficher cette date, nous pouvons utiliser les méthodes de l'objet ```Date```, telles que ```getDate()```, ```getMonth()```, ```getFullYear()``` et ```getHours()```. Voici un exemple de code qui affichera la date et l'heure actuelles dans le format "mm/dd/yyyy hh:mm" :

```TypeScript
const currentDate = new Date();
const month = currentDate.getMonth() + 1;
const day = currentDate.getDate();
const year = currentDate.getFullYear();
const hours = currentDate.getHours();
const minutes = currentDate.getMinutes();

console.log(`${month}/${day}/${year} ${hours}:${minutes}`);
```

Cela produira un résultat similaire à ceci : "4/30/2021 19:30". Nous pouvons également utiliser certaines méthodes pour obtenir la date et l'heure dans différents fuseaux horaires, ajouter ou soustraire des jours et même comparer les dates. La documentation officielle de TypeScript fournit des exemples complets et des explications détaillées sur toutes ces fonctionnalités.

## Analyse Approfondie

L'objet ```Date``` de TypeScript est basé sur l'objet ```Date``` de JavaScript, qui a été introduit dans ECMAScript 1 en 1997. Cependant, le support de l'objet ```Date``` dans les navigateurs peut varier légèrement et peut même présenter des bugs dans certains cas. Il existe également des bibliothèques tierces populaire, telles que Moment.js, qui offrent des fonctionnalités supplémentaires et une meilleure prise en charge des différentes zones horaires.

Cependant, avec les mises à jour récentes de TypeScript et la promotion d'un support fort des types au sein de la communauté, l'utilisation de bibliothèques tierces n'est plus aussi courante. Si vous devez gérer des dates dans différentes zones horaires, il peut être utile d'utiliser un package spécifique pour cela, tel que Date-fns, qui a une bonne prise en charge des types.

## Voir aussi

- Documentation officielle de TypeScript sur l'objet Date : https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#the-never-type
- Moment.js : https://momentjs.com/
- Date-fns : https://date-fns.org/