---
title:                "Calculer une date dans le futur ou le passé"
aliases: - /fr/google-apps-script/calculating-a-date-in-the-future-or-past.md
date:                  2024-02-01T21:48:49.669678-07:00
model:                 gpt-4-0125-preview
simple_title:         "Calculer une date dans le futur ou le passé"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Calculer une date dans le futur ou le passé consiste à manipuler des objets date pour trouver des dates au-delà ou avant la date actuelle, respectivement. Les programmeurs font cela pour des tâches allant de la configuration de rappels et de dates d'expiration à l'analyse des tendances des données basées sur le temps.

## Comment faire :

Dans Google Apps Script, qui est basé sur JavaScript, vous pouvez manipuler les dates en utilisant l'objet `Date`. Voici comment calculer des dates dans le futur et le passé :

### Calcul de la Date Future

Pour calculer une date future, vous créez un objet date pour la date actuelle, puis ajoutez le nombre souhaité de jours (ou toute autre unité de temps) à celle-ci.

```javascript
// Date actuelle
var today = new Date();

// Calculer une date 10 jours dans le futur
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Date Future : " + futureDate.toDateString());
```

### Calcul de la Date Passée

De manière similaire, pour trouver une date dans le passé, soustrayez le nombre de jours de la date actuelle.

```javascript
// Date actuelle
var today = new Date();

// Calculer une date 10 jours dans le passé
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Date Passée : " + pastDate.toDateString());
```

### Exemple de Sortie

Cela produirait quelque chose comme ce qui suit (en supposant qu'aujourd'hui soit le 15 avril 2023) :

```
Date Future : Mar 25 Avr 2023
Date Passée : Mer 05 Avr 2023
```

Rappelez-vous, l'objet Date en JavaScript (et donc dans Google Apps Script) ajuste automatiquement les mois et les années lorsque vous ajoutez ou soustrayez des jours.

## Plongée Profonde

La manipulation des dates avec l'objet `Date` découle des premières implémentations de JavaScript. Avec le temps, cette approche est généralement restée constante, offrant une manière directe pour les développeurs de gérer les dates sans nécessiter de bibliothèques externes. Toutefois, pour des opérations plus complexes comme les ajustements de fuseau horaire, ou lors du travail avec des données basées sur les dates étendues, des bibliothèques comme `Moment.js` ou le plus moderne `Luxon` pourraient offrir plus de fonctionnalités et une manipulation plus aisée.

Dans Google Apps Script, spécifiquement, malgré la disponibilité directe et la simplicité de l'objet `Date`, il est crucial de rester conscient de comment les calculs de dates peuvent impacter la performance du script et le temps d'exécution, particulièrement dans les déclencheurs pilotés par le temps ou les manipulations étendues de feuilles de calcul. De plus, bien que Google Apps Script fournisse des méthodes intégrées pour gérer les dates dans son écosystème (comme dans Google Sheets ou Calendar), intégrer des bibliothèques externes ou tirer parti des Services Avancés de Google peut parfois fournir des solutions plus robustes pour des scénarios complexes.

Ainsi, bien que la méthodologie de l'objet Date JavaScript natif soit généralement suffisante pour des calculs simples, explorer des bibliothèques ou services externes peut améliorer la fonctionnalité pour des besoins plus nuancés.
