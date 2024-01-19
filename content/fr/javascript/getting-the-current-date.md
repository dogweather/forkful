---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
Récupérer la date actuelle consiste à obtenir la date et l'heure exactes au moment de l'exécution du programme. Les programmeurs font cela pour des tâches diverses, allant du suivi du temps à générer des enregistrements de date / heure pour le journalisation, et même pour créer des fonctionnalités telles que des comptes à rebours.

## Comment faire :

Voici un petit bout de code qui démontre comment récupérer la date et l'heure actuelles en utilisant JavaScript.

```Javascript
let currentDate = new Date();
console.log(currentDate);
```
Cela renverra quelque chose comme:

```Javascript
2022-05-21T10:11:09.803Z
```
Ceci est le format d'horodatage ISO, qui est largement utilisé et accepté dans le monde de la programmation.

## Plongée profonde:

Historiquement, JavaScript a toujours eu l'objet Date intégré pour gérer tout ce qui concerne la date et l'heure. Cependant, des bibliothèques comme Moment.js sont apparues en réponse à la complexité de la gestion des dates et des heures dans différents fuseaux horaires. Ceci est en partie dû au fait que JavaScript utilise le fuseau horaire de l'utilisateur pour déterminer le temps actuel.

Une alternative à l'utilisation de l'objet Date ou de Moment.js est la bibliothèque day.js, qui est légère et similaire à Moment.js en termes de syntaxe et d'API, mais sans les dépendances volumineuses.

L'implémentation de `new Date()` en JavaScript crée une instance de l'objet Date JavaScript qui représente un seul moment dans le temps. Cet objet Date est basé sur la valeur temps Unix - le nombre de millisecondes écoulées depuis le 1er janvier 1970 00:00:00 UTC.

## Voir aussi: 

Pour plus de détails sur l'objet de la date JavaScript, consultez [Mozilla Developer Network](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)

Pour en savoir plus sur Moment.js et day.js:

- [Moment.js](https://momentjs.com/)
- [Day.js](https://day.js.org/)