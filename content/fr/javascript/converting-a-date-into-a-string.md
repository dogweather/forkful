---
title:                "Javascript: Convertir une date en chaîne de caractères"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous avons tous eu à traiter avec des dates dans nos projets de programmation. Mais parfois, il est nécessaire de convertir une date en chaîne de caractères pour afficher des informations plus précises à l'utilisateur. Dans cet article, nous allons découvrir comment convertir une date en chaîne de caractères en Javascript, et pourquoi cela peut être utile.

## Comment faire

Pour convertir une date en chaîne de caractères en Javascript, nous pouvons utiliser la méthode `toString()` de l'objet Date. Cette méthode renvoie une chaîne de caractères représentant la date et l'heure présentes dans l'objet Date.

```
// Création d'un objet Date avec la date et l'heure actuelles
let date = new Date();

// Utilisation de la méthode toString()
let dateString = date.toString();

// Affichage de la date sous forme de chaîne de caractères
console.log(dateString);
// Output: Fri Jul 09 2021 16:10:00 GMT+0200 (Central European Summer Time)
```

Nous pouvons également utiliser la méthode `toLocaleString()` pour obtenir une représentation de la date selon les paramètres locaux de l'utilisateur :

```
// Utilisation de la méthode toLocaleString()
let dateLocaleString = date.toLocaleString();

// Affichage de la date selon les paramètres locaux
console.log(dateLocaleString);
// Output: 9/07/2021 16:10:00
```

Il est également possible de formater la date selon un format spécifique en utilisant la librairie JavaScript Moment.js. Cette librairie permet de manipuler des dates et des heures de manière plus précise et offre différents formats de date prédéfinis.

```
// Utilisation de Moment.js pour formater la date
let formattedDate = moment(date).format("DD/MM/YYYY HH:mm:ss");

// Affichage de la date formatée
console.log(formattedDate);
// Output: 09/07/2021 16:10:00
```

## Plongée en profondeur

En Javascript, les dates sont représentées par un nombre de millisecondes écoulées depuis le 1er janvier 1970 à 00:00:00 UTC. Lorsque nous utilisons la méthode `toString()`, cette valeur est convertie en une chaîne de caractères lisible pour l'utilisateur.

La méthode `toLocaleString()` utilise les paramètres de localisation de l'utilisateur pour afficher la date sous une forme plus familière. Ce format peut varier selon les régions et les langues. Cependant, il peut être difficile de manipuler la date avec cette méthode car elle dépend des paramètres de localisation de chaque utilisateur.

En utilisant Moment.js, nous pouvons avoir plus de contrôle sur le format de la date, mais nous devons ajouter une dépendance supplémentaire à notre projet.

## Voir aussi

- [La documentation officielle de l'objet Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [La documentation de la méthode `toString()` en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/toString)
- [La documentation de la méthode `toLocaleString()` en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/toLocaleString)
- [La documentation de Moment.js](https://momentjs.com/docs/)