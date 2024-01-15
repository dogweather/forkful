---
title:                "Calculer une date dans le futur ou le passé."
html_title:           "Javascript: Calculer une date dans le futur ou le passé."
simple_title:         "Calculer une date dans le futur ou le passé."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà eu besoin de calculer une date dans le futur ou dans le passé, vous savez à quel point cela peut être utile. Que ce soit pour planifier un événement, créer des rappels ou simplement pour des besoins de programmation, il est important de savoir comment manipuler les dates en Javascript.

## Comment Faire

Pour calculer une date dans le futur ou dans le passé en Javascript, la première étape consiste à créer un objet "Date" en utilisant la syntaxe suivante : 
```Javascript
let date = new Date();
```
Ensuite, pour ajouter ou soustraire une période de temps à cette date, vous pouvez utiliser les méthodes suivantes : 
- Pour ajouter un certain nombre de jours : 
```Javascript
date.setDate(date.getDate() + 10); // ajoute 10 jours à la date actuelle
```
- Pour soustraire un certain nombre de jours : 
```Javascript
date.setDate(date.getDate() - 5); // soustrait 5 jours de la date actuelle
```
- Pour ajouter un certain nombre de mois : 
```Javascript
date.setMonth(date.getMonth() + 6); // ajoute 6 mois à la date actuelle
```
- Pour soustraire un certain nombre de mois : 
```Javascript
date.setMonth(date.getMonth() - 2); // soustrait 2 mois de la date actuelle
```
- Pour ajouter un certain nombre d'années : 
```Javascript
date.setFullYear(date.getFullYear() + 2); // ajoute 2 années à la date actuelle
```
- Pour soustraire un certain nombre d'années : 
```Javascript
date.setFullYear(date.getFullYear() - 5); // soustrait 5 années de la date actuelle
```

Vous pouvez également utiliser ces méthodes pour définir une date spécifique en utilisant les paramètres de l'objet Date. Par exemple, pour créer une date pour le 25 décembre 2020 : 
```Javascript
let date = new Date(2020, 11, 25); // le mois commence à 0, donc décembre est représenté par 11
```

## Plongée Profonde

Comme mentionné précédemment, le mois dans Javascript commence à 0 plutôt qu'à 1. Cela signifie qu'en utilisant la méthode "setMonth()", vous devrez soustraire 1 au mois réel. Par exemple, pour définir le mois de décembre, vous devrez utiliser la valeur 11 plutôt que 12.

De plus, il existe des méthodes supplémentaires telles que "setHours()", "setMinutes()", "setSeconds()" et "setMilliseconds()" qui vous permettent de préciser l'heure pour une date spécifique.

Enfin, pour obtenir le résultat sous forme de chaîne de caractères dans un format spécifique, vous pouvez utiliser la méthode "toLocaleDateString()" en spécifiant la langue et les options de format.

## See Also

- [Documentation sur les objets Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Manipuler les dates en Javascript](https://www.developpez.net/forums/d1922954/javascript/general-javascript/manipuler-dates-javascript/)