---
title:                "Javascript: Calculer une date dans le futur ou le passé"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Le calcul d'une date dans le futur ou dans le passé peut être une tâche très utile en programmation. Cela permet de créer des fonctionnalités telles que des rappels ou des mises à jour automatiques de données. Cela peut également aider à la planification et à la gestion du temps dans les applications.

## Comment faire

Pour effectuer des calculs de date en Javascript, il existe plusieurs options. La première consiste à utiliser la classe Date intégrée, qui représente une date et une heure spécifiques selon la norme du temps Unix. Voici un exemple du calcul d'une date dans le futur en utilisant cette méthode :

```Javascript
// Définition de la date actuelle
let dateActuelle = new Date();

// Ajouter 1 mois à la date actuelle
let dateFutur = new Date(dateActuelle.getFullYear(), dateActuelle.getMonth() + 1, dateActuelle.getDate());

console.log(dateFutur); // Output: la date dans un mois à partir de maintenant
```

Vous pouvez également utiliser des bibliothèques externes telles que Moment.js pour simplifier les calculs de date en Javascript. Recherchez et choisissez celle qui convient le mieux à vos besoins.

## Plongée en profondeur

Pour comprendre pleinement comment fonctionnent les calculs de date en Javascript, il est important de connaître le système de temps utilisé par cette langue. Le temps Unix est un système basé sur le nombre de secondes écoulées depuis le 1er janvier 1970 à minuit GMT. Il est important de noter que Javascript utilise le fuseau horaire local de l'utilisateur pour ses calculs de date, ce qui peut entraîner des résultats différents pour les utilisateurs dans différents fuseaux horaires.

Lors du calcul de dates dans le futur ou dans le passé, il est important d'être conscient des problèmes liés aux années bissextiles et aux fuseaux horaires. Vous pouvez trouver des solutions à ces problèmes dans des bibliothèques telles que Moment.js ou en utilisant des techniques de validation et de conversion de fuseaux horaires.

## Voir aussi

- [Documentation sur la classe Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Moment.js - Bibliothèque de manipulation de dates en Javascript](https://momentjs.com/)
- [Article sur la gestion des dates en Javascript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-javascript)