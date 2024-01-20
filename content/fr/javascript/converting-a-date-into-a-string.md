---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

La conversion d'une date en chaîne de caractères implique de changer le format d'une date (qui est un objet) en une série de caractères. Les programmeurs le font pour faciliter l'affichage et le stockage des dates.

## Comment faire:

Voici comment vous pouvez convertir une date en chaîne de caractères en Javascript:
```Javascript
let maDate = new Date();
let dateChaine = maDate.toString();
console.log(dateChaine); // Affiche "Fri May 29 2020 14:52:50 GMT+0200 (Central European Summer Time)"
```
La fonction `toString()` convertit l'objet Date en une chaîne de caractères.

## Approfondissement

Historiquement, la conversion d'une date en une chaîne est une pratique courante en programmation depuis la création de JavaScript en 1995. Cela permet de manipuler et d'afficher facilement les dates dans le format souhaité.

Il existe des alternatives à la méthode `toString()`. Par exemple, `toDateString()` qui produit un résultat plus lisible:
```Javascript
let maDate = new Date();
let dateChaine = maDate.toDateString();
console.log(dateChaine); // Affiche "Fri May 29 2020"
```
Lors de l'implémentation, il est important de considérer l'effet des fuseaux horaires et des paramètres locaux sur les dates. Parfois, il est nécessaire d'utiliser la méthode `toUTCString()` pour normaliser les données de date.

## Voir Aussi

- Documentation MDN sur l'objet Date: [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)

- Article sur les méthodes de l'objet Date: [http://javascript.info/date#toString](http://javascript.info/date#toString)

- Un guide pour travailler avec les dates et les heures en Javascript: [https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)