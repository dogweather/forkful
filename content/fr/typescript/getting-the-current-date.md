---
title:                "Obtenir la date actuelle"
html_title:           "TypeScript: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La fonctionnalité de récupération de la date courante est essentielle pour les développeurs TypeScript lorsqu'ils travaillent avec des applications web ou des projets nécessitant une gestion du temps. Elle permet de manipuler facilement les données liées à la date et de les mettre à jour en temps réel.

## Comment Faire

Pour récupérer la date courante en TypeScript, il suffit d'utiliser la méthode `Date()` fournie par le langage. Cette méthode renvoie un objet Date contenant la date et l'heure actuelles.

```TypeScript
let date = new Date();
console.log(date);
```

Output:
`2021-09-22T14:21:38.070Z`

Il est également possible de spécifier une date et une heure spécifiques en utilisant les paramètres de la méthode `Date()`.

```TypeScript
let date = new Date(2021, 8, 22, 14, 30);
console.log(date);
```

Output:
`2021-09-22T12:30:00.000Z`

Pour formater la date et l'heure en fonction de ses besoins, il est recommandé d'utiliser la bibliothèque Moment.js qui est compatible avec TypeScript.

## Plongée Profonde

En TypeScript, la méthode `Date()` repose sur la bibliothèque JavaScript du même nom, qui prend en compte le fuseau horaire local de l'utilisateur. Ainsi, si votre application est utilisée dans différents fuseaux horaires, il est important de tenir compte de ce paramètre pour éviter les erreurs liées à la date.

De plus, la méthode `Date()` renvoie non seulement la date et l'heure actuelles, mais également les millisecondes écoulées depuis le 1er janvier 1970. Ce nombre est appelé "timestamp" et peut être utile pour effectuer des calculs de temps dans votre application.

## Voir Aussi

- [La documentation officielle sur la méthode Date() en TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#date)
- [La documentation de Moment.js](https://momentjs.com/docs/)