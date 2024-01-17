---
title:                "Comparaison de deux dates"
html_title:           "Javascript: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Qu'est-ce et pourquoi?

Comparer deux dates est une tâche courante en programmation. Cela permet de déterminer si une date est antérieure, postérieure ou égale à une autre. Les programmeurs utilisent cela pour trier et filtrer des données, ainsi que pour gérer des tâches planifiées.

# Comment faire:

Voici comment comparer deux dates en Javascript:

```Javascript
// Définir deux dates:
let date1 = new Date('2021-01-01');
let date2 = new Date('2021-02-15');

// Utiliser l'opérateur de comparaison ">" pour déterminer si date1 est antérieure à date2:
if (date1 < date2) {
  console.log('date1 est antérieure à date2');
}
// Output: date1 est antérieure à date2
```

Voici un autre exemple avec des dates égales:

```Javascript
// Définir deux dates égales:
let date3 = new Date('2021-03-06');
let date4 = new Date('2021-03-06');

// Utiliser l'opérateur de comparaison "===" pour déterminer si les dates sont égales:
if (date3 === date4) {
  console.log('date3 et date4 sont égales');
}
// Output: date3 et date4 sont égales
```

# Plongée en profondeur:

Historiquement, les dates ont été représentées en tant que nombres de millisecondes depuis le 1er janvier 1970, appelé "Epoch time". Cependant, cela a été remplacé en Javascript par un objet spécial appelé "Date". 

Une alternative à l'utilisation des opérateurs de comparaison est la méthode "getTime()", qui renvoie le nombre de millisecondes écoulées depuis l'Epoch time.

Il est important de noter que deux dates ne seront considérées comme égales que si elles ont exactement la même valeur en millisecondes. Même une différence d'une milliseconde entraînera une évaluation comme étant fausse.

# Voir aussi:

Pour en savoir plus sur les dates en Javascript, consultez ces ressources supplémentaires:

- La documentation officielle de Mozilla: [MDN Web Docs - Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- Un tutoriel pratique sur la comparaison de dates en Javascript: [How to Compare Dates in JavaScript](https://www.date-course.com/date-comparison) (en anglais)