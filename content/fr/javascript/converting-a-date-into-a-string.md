---
title:                "Transformer une date en chaîne de caractères"
html_title:           "Javascript: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertissez une date en chaîne de caractères en Javascript pour afficher une date dans un format lisible et personnalisé, adapté à vos besoins. Cela peut être utile pour les applications de calendrier, les pages web avec des fonctionnalités de réservation ou pour simplement afficher une date dans un format convivial pour les utilisateurs.

## Comment faire

```Javascript
// Créez un nouvel objet Date avec la date souhaitée
let today = new Date();

// Utilisez les méthodes getMonth(), getDate(), et getFullYear() pour obtenir les informations de la date
let month = today.getMonth() + 1; //ajoute 1 car les mois en Javascript commencent à 0
let day = today.getDate();
let year = today.getFullYear();

// Utilisez une chaîne de caractères pour définir le format souhaité
let dateString = `${month}/${day}/${year}`;

// Affichez la date sous forme de chaîne de caractères personnalisée
console.log(dateString); //output: 5/25/2021
```

Il est également possible de modifier le format de la date en utilisant les méthodes `getHours()`, `getMinutes()` et `getSeconds()` pour afficher l'heure et les minutes. Vous pouvez également ajouter du texte ou d'autres éléments à la chaîne de caractères pour avoir un format plus complet.

## Deep Dive

Les objets Date en Javascript stockent la date et l'heure sous forme de nombre, ce qui peut sembler complexe à première vue. Cependant, en utilisant les méthodes `getFullYear()`, `getMonth()`, `getDate()`, `getHours()`, `getMinutes()` et `getSeconds()`, vous pouvez facilement extraire ces informations et les utiliser pour créer votre propre format de date en chaîne de caractères.

Il est également important de noter que le format de la date peut varier en fonction de la localisation de l'utilisateur. Par exemple, en Europe, la date est généralement écrite avec le jour avant le mois, tandis qu'aux Etats-Unis, c'est l'inverse. Vous pouvez utiliser la méthode `toLocaleDateString()` pour adapter la date au format utilisé dans la région de l'utilisateur.

## Voir aussi

- [Documentation officielle sur les objets Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Convertir une chaîne de caractères en objet Date en Javascript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Affichage de la date et de l'heure en Javascript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-javascript-fr)