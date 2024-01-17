---
title:                "Comparaison de deux dates"
html_title:           "TypeScript: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la comparaison de deux dates et pourquoi les programmeurs le font-ils?
La comparaison de deux dates est une méthode qui permet de comparer deux valeurs de type date pour déterminer leur relation chronologique. Les programmeurs l'utilisent souvent pour trier des informations dans un ordre chronologique, pour vérifier si une date est supérieure ou inférieure à une autre, ou pour effectuer des opérations de calcul de temps.
## Comment faire:
Voici un exemple de code TypeScript montrant comment comparer deux dates et afficher le résultat:
```
// Déclaration de deux variables de type Date
let date1: Date = new Date('1/1/2021');
let date2: Date = new Date('1/1/2022');

// Utilisation de l'opérateur logique ">" pour comparer les deux dates
if (date1 > date2) {
  console.log(`${date1} est après ${date2}`);
} else {
  console.log(`${date1} est avant ${date2}`);
}
```
Résultat:
```
1/1/2022 est après 1/1/2021
```
## Approfondissement:
La comparaison de dates est une opération courante dans la programmation, en particulier dans les applications qui traitent des données temporelles telles que les calendriers, les tâches planifiées, les réservations, etc. Il existe plusieurs façons de comparer des dates, telles qu'en utilisant des méthodes de comparaison prédéfinies ou en convertissant les dates en numéro de série puis en les comparant.
## Voir aussi:
- [Documentation officielle de TypeScript sur les dates](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Article sur la comparaison de dates en JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Guide de programmation pour les dates en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-the-date-object-in-typescript)