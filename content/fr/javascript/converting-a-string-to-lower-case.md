---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "Javascript: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La conversion d'une chaîne de caractères en minuscules est un moyen courant pour les programmeurs d'assurer l'homogénéité dans leurs données. En convertissant une chaîne en minuscules, tous les caractères seront facilement comparables et recherchables, indépendamment de leur casse d'origine.

## Comment faire:

```Javascript
let chaine = "Exemple EN majuscules";
let chaineMin = chaine.toLowerCase();
console.log(chaineMin); // affiche "exemple en majuscules"
```

Dans l'exemple ci-dessus, la méthode `toLowerCase()` est utilisée pour convertir la chaîne en minuscules. Cette méthode est intégrée à Javascript, donc pas besoin d'installer un module supplémentaire pour l'utiliser.

## Profondeur:

La prise en compte de la casse dans les données n'est pas une tâche nouvelle pour les programmeurs. Avant l'arrivée de la méthode `toLowerCase()`, il était nécessaire de parcourir manuellement les chaînes de caractères pour les convertir en minuscules. Aujourd'hui, il existe également une méthode similaire appelée `toUpperCase()` pour convertir une chaîne en majuscules.

Si vous travaillez avec des caractères unicode, il peut être utile de connaître la différence entre `toLowerCase()` et `toLocaleLowerCase()`. La méthode `toLocaleLowerCase()` utilise les règles de casse spécifiques à la langue de l'utilisateur, tandis que `toLowerCase()` utilise les règles de casse génériques.

## Voir aussi:

Documentation officielle pour `toLowerCase()`:
https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase