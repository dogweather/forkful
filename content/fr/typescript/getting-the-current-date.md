---
title:    "TypeScript: Obtenir la date actuelle"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi 
Obtenir la date actuelle et l'utiliser dans nos programmes est une tâche courante en programmation. Cela nous permet de suivre le temps, d'enregistrer des événements et de créer des fonctionnalités basées sur la date. Dans cet article, nous allons plonger dans la façon d'obtenir la date actuelle en TypeScript et comment l'utiliser dans vos projets.

## Comment faire 
L'une des façons les plus simples d'obtenir la date actuelle en TypeScript est d'utiliser la fonction `Date()`. Voici un exemple de code qui illustre comment l'utiliser :

```TypeScript
let date = new Date();
console.log(date);
```

La sortie de ce code sera quelque chose comme ceci : `2021-09-22T16:20:30.000Z`, ce qui représente la date et l'heure actuelles sous forme de chaîne de caractères en format ISO.

Pour obtenir la date dans un format plus lisible pour les humains, nous pouvons utiliser les méthodes `getDate()`, `getMonth()` et `getFullYear()` pour obtenir respectivement le jour, le mois et l'année de la date actuelle. Voici un exemple de code qui illustre cela :

```TypeScript
let date = new Date();
let jour = date.getDate();
let mois = date.getMonth() + 1; // Les mois commencent à partir de 0, donc on ajoute 1
let annee = date.getFullYear();

console.log(`${jour}/${mois}/${annee}`); // affiche la date au format JJ/MM/AAAA
```

La sortie de ce code sera quelque chose comme ceci : `22/09/2021`.

Nous pouvons également utiliser des bibliothèques externes telles que Moment.js pour un formatage de dates plus avancé.

## Plongée en profondeur 
La fonction `Date()` a également des paramètres facultatifs pour spécifier une date et heure spécifiques plutôt que d'obtenir la date actuelle. Par exemple, `new Date('2021-01-01')` créera une date représentant le 1er janvier 2021. Nous pouvons également fournir des valeurs pour les heures, minutes, secondes et millisecondes.

Il est important de noter que les dates et heures sont gérées différemment selon les fuseaux horaires, donc si vous avez besoin de travailler avec des dates spécifiques à une région, vous devrez peut-être utiliser une bibliothèque qui gère cela.

## Voir également 
- [Documentation officielle sur l'objet Date de TypeScript](https://www.typescriptlang.org/docs/handbook/built-in-objects.html#date)
- [Moment.js](https://momentjs.com/) pour une manipulation de dates plus avancée en TypeScript