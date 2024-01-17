---
title:                "Obtenir la date actuelle"
html_title:           "Javascript: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Obtenir la date actuelle est une tâche courante pour les programmeurs. Cela leur permet de connaître l'heure exacte à laquelle une partie de code est exécutée, ce qui peut être utile pour le débogage et pour enregistrer l'heure de création de données.

## Comment faire:

```Javascript
let today = new Date(); //crée un nouvel objet Date avec la date et l'heure actuelles
console.log(today); //affiche la date actuelle dans la console 
```

L'exemple ci-dessus utilise la méthode `new Date()` pour créer un objet Date avec la date et l'heure actuelles. En utilisant la méthode `getMonth()`, `getDate()`, `getFullYear()` et `getHours()`, vous pouvez obtenir des informations plus précises sur la date et l'heure en cours. 

## Plongée Profonde:

L'obtention de la date actuelle est une fonctionnalité courante dans les langages de programmation. Elle est initialement apparue en 1970 avec le langage de programmation C et est maintenant disponible dans la plupart des langages courants. Une alternative pour obtenir la date actuelle est d'utiliser un service externe, tel que l'API de Google pour obtenir l'heure mondiale.

## Voir Aussi:

Pour en savoir plus sur l'utilisation de la date actuelle en Javascript, vous pouvez consulter la documentation officielle de [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) ou regarder des tutoriels en ligne sur YouTube.