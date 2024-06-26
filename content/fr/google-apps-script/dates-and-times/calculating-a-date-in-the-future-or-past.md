---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:48:49.669678-07:00
description: "Comment faire : Dans Google Apps Script, qui est bas\xE9 sur JavaScript,\
  \ vous pouvez manipuler les dates en utilisant l'objet `Date`. Voici comment calculer\u2026"
lastmod: '2024-03-13T22:44:57.202335-06:00'
model: gpt-4-0125-preview
summary: "Dans Google Apps Script, qui est bas\xE9 sur JavaScript, vous pouvez manipuler\
  \ les dates en utilisant l'objet `Date`."
title: "Calculer une date dans le futur ou le pass\xE9"
weight: 26
---

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
