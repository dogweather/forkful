---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Manipulation de la date courante en TypeScript: un guide simplifié

## Quoi et pourquoi ?

Obtenir la date courante en programmation fait référence à l'extraction des informations de calendrier précises à l'instant présent. Pourquoi les programmeurs font-ils cela ? C'est essentiel pour suivre les événements, marquer les enregistrements de la base de données, traiter les transactions et plus encore.

## Comment faire :

En TypeScript, on peut obtenir la date et l'heure actuelles via l'objet intégré `Date`. Voici comment ça fonctionne:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

La sortie sera quelque chose comme:

```TypeScript
2022-03-01T08:50:00.000Z
```

Notez que TypeScript retourne toujours la date et l'heure au format UTC. 

## Plongée en profondeur:

L'objet `Date` existe depuis les premiers jours de JavaScript, et TypeScript, en tant que surcouche de JavaScript, hérite de toutes ses fonctionnalités.

Il existe des alternatives pour obtenir la date courante comme les bibliothèques de dates externes telles que `moment.js` ou `day.js`, qui fournissent des fonctionnalités supplémentaires pour la manipulation des dates.

Concernant les détails d'implémentation, l'objet `Date` de TypeScript accède à l'horloge système de l'ordinateur pour obtenir les informations actuelles de date et d'heure.

## Voir aussi :

1. Le guide officiel de Mozilla sur l'objet [`Date`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date) en JavaScript.
2. Documentation officielle de [`moment.js`](https://momentjs.com/docs/).
3. Documentation officielle de [`day.js`](https://day.js.org/).