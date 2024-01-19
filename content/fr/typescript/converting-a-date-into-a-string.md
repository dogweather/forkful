---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

Convertir une date en chaîne de caractères permet de la représenter sous un format lisible par un humain. Les programmeurs le font principalement pour afficher des dates dans des interfaces utilisateur ou pour des opérations de journalisation.

## Comment faire :

Voici un exemple en TypeScript qui montre comment convertir une date en chaîne de caractères.

```TypeScript
let dateActuelle = new Date();
let dateTexte = dateActuelle.toString();

console.log(dateTexte);
```

Cela produira un résultat similaire à :

```bash
Tue Sep 24 2021 10:20:42 GMT+0300 (heure de l’Europe de l’Est)
```

## Plus de détails

Historiquement, en JavaScript et donc aussi en TypeScript, la méthode `toString()` retourne une chaîne de caractères représentant la date en format anglo-saxon, avec le jour de la semaine, le mois, le jour du mois, l'année, et l'heure.

Des alternatives existent, comme la méthode `toDateString()` qui ne renvoie que la date (sans l'heure), ou `toLocaleString()`, qui adapte le format à la localisation de l'utilisateur.

A noter que ces méthodes font partie de l'objet Date de JavaScript, et donc héritées par TypeScript qui est un sur-ensemble de JavaScript. Elles dépendent de l'implémentation de l'environnement d'exécution.

Example:

```TypeScript
let dateActuelle = new Date();
console.log(dateActuelle.toDateString());
console.log(dateActuelle.toLocaleString());
```

Le résultat serait :

```bash
Tue Sep 24 2021
24/09/2021, 10:20:42
```

## Pour aller plus loin

Voici quelques liens pour approfondir le sujet.

- La documentation Mozilla sur l'objet `Date` en JavaScript : https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date
- Une explication plus détaillée des fonctions de conversion de date en chaîne de caractères : https://www.w3schools.com/jsref/jsref_obj_date.asp
- La documentation officielle de TypeScript : https://www.typescriptlang.org/docs/