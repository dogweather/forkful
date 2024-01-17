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

## Qu'est-ce que c'est et pourquoi il est important?

Calculer une date dans le futur ou dans le passé consiste à déterminer une date qui se situe après ou avant une date spécifique donnée. Les programmeurs utilisent souvent cette fonctionnalité pour effectuer des tâches telles que planifier des événements ou gérer des abonnements. Cela leur permet de créer des programmes dynamiques et de prendre en compte l'évolution du temps.

## Comment faire:

Voici un exemple de code utilisant la fonction ```Date()``` en JavaScript pour calculer la date d'anniversaire d'un utilisateur dans 10 ans:

```
let userBirthday = new Date();
userBirthday.setFullYear(userBirthday.getFullYear() + 10);
console.log(userBirthday.toLocaleDateString());
```

L'output sera la date dans 10 ans au format mm/jj/aaaa.

## En savoir plus:

La capacité de calculer des dates dans le futur ou dans le passé est une fonctionnalité importante dans le développement de logiciels et de sites web. Cela permet aux programmeurs de créer des applications plus interactives et personnalisées pour les utilisateurs. Dans le passé, les programmeurs devaient effectuer ces calculs manuellement, mais avec l'avènement des langages de programmation modernes, il est désormais possible d'utiliser des fonctions natives comme ```Date()``` en JavaScript pour faciliter le processus.

Il existe également des alternatives à la fonction ```Date()``` en JavaScript, telles que Moment.js ou Luxon, qui offrent des fonctionnalités avancées pour la gestion des dates et des heures.

En ce qui concerne les détails de mise en œuvre, il est important de comprendre comment les ordinateurs stockent les dates et les heures en tant que valeurs numériques. En utilisant ces valeurs, les programmeurs peuvent effectuer des calculs pour trouver la date souhaitée dans le futur ou dans le passé.

## Voir aussi:

Pour en savoir plus sur la gestion du temps en JavaScript, vous pouvez consulter ces ressources:

- [Date objects in JavaScript (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js documentation](https://momentjs.com/docs/)
- [Luxon documentation](https://moment.github.io/luxon/)