---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Javascript: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

Calculer une date dans le futur ou le passé consiste à manipuler une date existante et à y ajouter ou à en soustraire un certain temps. Les développeurs doivent faire cela pour de nombreuses raisons, comme planifier des événements, définir des délais et suivre le temps écoulé.

## Comment faire:

Dans Javascript, nous avons la classe Date intégrée qui rend cette opération assez simple. Voici quelques exemples de manipulation de dates.

Pour calculer une date dans le futur :

```Javascript
let maintenant = new Date();
maintenant.setDate(maintenant.getDate() + 5);  // Ajouter 5 jours à la date actuelle

console.log(maintenant);
```

Ou une date dans le passé :

```Javascript
let maintenant = new Date();
maintenant.setDate(maintenant.getDate() - 5);  // Soustraire 5 jours de la date actuelle

console.log(maintenant);
```

## Approfondissement :

Historiquement, calculer une date dans le futur ou le passé était beaucoup plus complexe qu'il ne l'est aujourd'hui. Auparavant, les développeurs devaient comprendre les subtilités des calendriers, des fuseaux horaires et des dates bissextiles. Maintenant, grâce à la classe Date de Javascript, nous avons toutes ces fonctionnalités géniales prêtes à l'emploi.

Une alternative à l'utilisation de la classe Date pourrait être l'usage des bibliothèques tierces, comme Moment.js. Ces bibliothèques offrent plus de flexibilité, et sont spécialement utiles lors de la manipulation de dates et de fuseaux horaires.

En ce qui concerne les détails d'implémentation, il est essentiel de comprendre que la méthode setDate() modifie directement l'objet Date sur lequel elle est appelée. Cela signifie que la date originale est perdue en conséquence.

## À Voir Également :

- Documentation sur la Classe Date : [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)

- Introduction à Moment.js : [https://momentjs.com/docs/](https://momentjs.com/docs/) 

- Informations plus détaillées sur les dates et les heures en JavaScript : [https://flaviocopes.com/javascript-dates/](https://flaviocopes.com/javascript-dates/)