---
title:                "Javascript: Obtenir la date actuelle"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle dans un programme JavaScript peut sembler banal, mais c'est en fait une étape importante pour de nombreux projets. La date actuelle peut être utilisée pour afficher l'heure de publication d'un article, pour suivre le temps écoulé depuis une certaine date ou pour créer des fonctionnalités basées sur le temps telles que des rappels ou des compteurs.

## Comment faire

Il existe plusieurs façons d'obtenir la date actuelle en JavaScript. Voici deux exemples:

```Javascript
// Utiliser l'objet Date pour créer une instance de la date actuelle
let currentDate = new Date();

// Utiliser la méthode Date.now() pour obtenir la date actuelle en millisecondes depuis le 1er janvier 1970
let currentDateInMs = Date.now();

console.log(currentDate); // Affiche la date complète, y compris l'heure et la zone horaire
console.log(currentDateInMs); // Affiche la date en millisecondes

// Pour formater la date selon vos préférences, vous pouvez utiliser des méthodes telles que .getFullYear(), .getMonth() ou .toLocaleDateString()
console.log(currentDate.toLocaleDateString()); // Affiche la date au format MM/JJ/YYYY
```

## Plongez plus en profondeur

La méthode Date.now() peut sembler simple, mais elle repose en fait sur un concept appelé "timestamp". Ce timestamp est le nombre de millisecondes écoulées depuis le 1er janvier 1970, également connu sous le nom de Epoch ou UNIX time. En utilisant ce timestamp, les appareils peuvent facilement suivre le temps écoulé sans avoir à se soucier des zones horaires ou des changements de date.

Pour en savoir plus sur les différentes méthodes de l'objet Date et le fonctionnement des timestamps, vous pouvez consulter la documentation officielle de JavaScript.

## Voir aussi

- [Documentation de JavaScript sur l'objet Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Timestamp et Epoch time expliqués](https://www.epochconverter.com/)
- [Formater les dates en JavaScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-javascript)

Merci d'avoir lu cet article sur l'obtention de la date actuelle en JavaScript. Nous espérons que cela vous sera utile dans vos projets futurs !