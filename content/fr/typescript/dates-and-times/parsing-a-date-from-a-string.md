---
title:                "Analyser une date depuis une chaîne de caractères"
aliases:
- /fr/typescript/parsing-a-date-from-a-string/
date:                  2024-02-03T19:15:54.193560-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne de caractères consiste à convertir des représentations textuelles de dates et d'heures en un format qui peut être manipulé et analysé par le programme. Il s'agit d'une tâche courante en programmation car elle permet de gérer les entrées des utilisateurs, de stocker des données horodatées et d'interagir avec des API, donnant lieu à des applications plus fonctionnelles et conviviales.

## Comment faire :
TypeScript, étant un sur-ensemble de JavaScript, s'appuie sur l'objet Date pour analyser les dates à partir de chaînes. Toutefois, travailler avec les dates en JS/TS peut devenir verbeux ou imprécis en raison des particularités de l'objet Date. Voici un exemple basique suivi d'une approche utilisant une bibliothèque populaire, `date-fns`, pour des solutions plus robustes.

### Utilisation de l'objet Date de JavaScript
```typescript
// Analyse basique en utilisant le constructeur Date
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// Résultat pour GMT : "Fri Apr 21 2023 15:00:00 GMT+0000 (Temps Universel Coordonné)"
```

Cette méthode fonctionne pour les chaînes au format ISO et certains autres formats de date, mais peut donner des résultats incohérents pour les formats ambigus à travers les navigateurs et les locales.

### Utilisation de date-fns
La bibliothèque `date-fns` offre une gestion des dates directe et cohérente. C'est une bibliothèque modulaire, vous permettant d'inclure uniquement les parties dont vous avez besoin, réduisant ainsi la taille du bundle.

Tout d'abord, installez `date-fns` : 

```sh
npm install date-fns
```

Ensuite, utilisez-la pour analyser une chaîne de date :

```typescript
import { parseISO, format } from 'date-fns';

// Analyse d'une chaîne ISO
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// Formatage de la date (par ex., en une forme lisible par l'homme)
console.log(format(parsedDate, "PPPpp")); 
// Résultat : "21 avril 2023 à 15:00" (le résultat peut varier selon la locale)
```

`date-fns` supporte une grande variété de formats et de locales, ce qui en fait un choix robuste pour les applications nécessitant une analyse et un formatage précis des dates à travers différentes régions d'utilisateurs.
