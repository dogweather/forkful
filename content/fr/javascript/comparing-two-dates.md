---
title:                "Javascript: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Pourquoi

Comparer deux dates est une tâche courante dans la programmation Javascript. Cela peut être utile pour vérifier si une date est antérieure ou ultérieure à une autre, pour calculer la différence entre deux dates, ou pour trier des données en fonction des dates. Comprendre comment comparer deux dates peut améliorer vos compétences en programmation et vous permettre de créer des fonctions plus avancées.

##Comment faire

Pour comparer deux dates en Javascript, nous pouvons utiliser l'opérateur de comparaison ">", "<", ">=", "<=" ou la méthode `getTime()`. Voyons quelques exemples concrets:

```Javascript
// Créer deux dates à comparer
const date1 = new Date("October 6 2021");
const date2 = new Date("October 15 2021");

// Utiliser l'opérateur de comparaison pour vérifier si date1 est antérieure à date2
console.log(date1 < date2); // Output: true

// Utiliser la méthode getTime() pour obtenir le nombre de millisecondes depuis le 1er janvier 1970
const time1 = date1.getTime();
const time2 = date2.getTime();

// Comparer les nombres de millisecondes
if (time1 > time2) {
  console.log("La date 1 est ultérieure à la date 2");
} else {
  console.log("La date 1 est antérieure à la date 2");
}
// Output: La date 1 est antérieure à la date 2
```

Dans cet exemple, nous avons utilisé l'opérateur de comparaison `<` pour vérifier si la date1 est antérieure à la date2. Nous avons également utilisé la méthode `getTime()` pour comparer les dates en utilisant leur valeur en millisecondes.

##Plongée en profondeur

Lorsque nous comparons des dates en Javascript, il est important de comprendre comment les dates sont représentées dans le langage. En Javascript, les dates sont stockées en tant que nombres de millisecondes écoulées depuis le 1er janvier 1970 à minuit GMT. Cela signifie que les dates antérieures à cette date auront une valeur en millisecondes négative et les dates ultérieures auront une valeur positive.

De plus, lors de la comparaison, Javascript convertit les dates en nombre de millisecondes avant de les comparer. Cela peut entraîner des résultats inattendus si les dates ont des heures différentes. Par exemple, la date "October 6 2021, 00:00:00" sera considérée comme étant antérieure à la date "October 6 2021, 12:00:00", même si elles sont le même jour.

Il est donc important de prendre en compte ces différences lors de la comparaison de dates en Javascript.

##Voir aussi

- [Documentation MDN sur les objets Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Comparaison de dates en Javascript](https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-javascript)
- [Opérateurs de comparaison en Javascript](https://www.tutsmake.com/compare-dates-javascript/#JavaScript_Operators_for_Date_Comparison)

En utilisant les connaissances que nous avons acquises dans cet article, vous serez en mesure de comparer efficacement des dates en Javascript et de mieux comprendre le fonctionnement du langage. Cela vous aidera à améliorer vos compétences en programmation et à créer des fonctions plus avancées dans vos projets. N'hésitez pas à consulter les liens ci-dessus pour en savoir plus sur les objets Date et les opérateurs de comparaison en Javascript.