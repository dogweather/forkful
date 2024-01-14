---
title:                "TypeScript: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

La conversion d'une date en chaîne de caractères est une tâche courante en programmation pour pouvoir afficher la date dans un format lisible par les utilisateurs. Cela peut également être utile pour faciliter les opérations de tri ou de comparaison de dates. 

## Comment faire 

```TypeScript
// Exemple de conversion d'une date en chaîne de caractères au format MM/JJ/AAAA
let date = new Date(); // Récupération de la date actuelle
let month = date.getMonth() + 1; // Obtention du mois actuel et ajout de 1 car les mois en JavaScript commencent à 0
let day = date.getDate(); // Obtention du jour actuel
let year = date.getFullYear(); // Obtention de l'année actuelle
let dateString = `${month}/${day}/${year}`; // Concaténation des valeurs pour obtenir la chaîne de caractères finale
console.log(dateString); // Résultat : 07/15/2021
```

## Plongée plus profonde 

La conversion d'une date en chaîne de caractères peut sembler simple, mais il est important de prendre en compte certaines choses comme les différents formats de date dans différentes langues et régions, le fuseau horaire, et la prise en compte des années bissextiles. En utilisant des bibliothèques de manipulation de dates comme Moment.js, ces problèmes peuvent être résolus de manière simplifiée et fiable. 

## Voir aussi

- [Documentation JavaScript - Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Moment.js](https://momentjs.com/)
- [Tuto TypeScript - Convertir une date en chaîne de caractères](https://www.digitalocean.com/community/tutorials/how-to-convert-a-date-object-into-a-string-in-typescript-and-javascript)