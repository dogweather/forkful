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

## Pourquoi

Comparer deux dates est un élément essentiel de la programmation, que ce soit dans le cadre d'une application ou pour résoudre tout problème lié au temps. Les dates sont utilisées pour suivre le temps écoulé, planifier des événements et bien plus encore. La capacité de comparer deux dates est donc une compétence importante pour tout développeur.

## Comment faire

Pour comparer deux dates en TypeScript, nous pouvons utiliser l'objet Date inclus dans JavaScript. Cet objet nous permet de créer des instances qui représentent des dates précises et offre également des méthodes pour effectuer des opérations sur ces dates.

Voici un exemple de code TypeScript pour comparer deux dates :

```TypeScript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-02-01");

if (date1 > date2) {
  console.log("La date 1 est plus récente que la date 2");
} else {
  console.log("La date 2 est plus récente que la date 1");
}
```

L'objet Date a une méthode `getTime()` qui renvoie le nombre de millisecondes écoulées depuis le 1er janvier 1970. En utilisant cette méthode, nous pouvons comparer des dates en soustrayant le résultat de `getTime()` de deux dates différentes.

```TypeScript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-02-01");

if (date1.getTime() > date2.getTime()) {
  console.log("La date 1 est plus récente que la date 2");
} else {
  console.log("La date 2 est plus récente que la date 1");
}
```

Nous pouvons également utiliser les méthodes `getFullYear()`, `getMonth()`, `getDate()` et `getDay()` pour comparer deux dates en comparant chaque élément individuellement.

```TypeScript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-02-01");

if (date1.getFullYear() > date2.getFullYear()) {
  console.log("La date 1 est plus récente que la date 2");
} else if (date1.getFullYear() < date2.getFullYear()) {
  console.log("La date 2 est plus récente que la date 1");
} else {
  if (date1.getMonth() > date2.getMonth()) {
    console.log("La date 1 est plus récente que la date 2");
  } else if (date1.getMonth() < date2.getMonth()) {
    console.log("La date 2 est plus récente que la date 1");
  } else {
    if (date1.getDate() > date2.getDate()) {
      console.log("La date 1 est plus récente que la date 2");
    } else if (date1.getDate() < date2.getDate()) {
      console.log("La date 2 est plus récente que la date 1");
    } else {
      console.log("Les deux dates sont identiques");
    }
  }
}
```

## Plongée en profondeur

La comparaison de deux dates peut sembler simple, mais il y a quelques points à garder à l'esprit lors de l'utilisation de l'objet Date en TypeScript. Tout d'abord, l'objet Date utilise le fuseau horaire du système sur lequel il est exécuté. Cela signifie que si votre application est déployée dans différents fuseaux horaires, les dates peuvent différer.

Deuxièmement, si vous avez besoin de comparer des dates avec une précision supérieure à la journée, vous devrez utiliser d'autres méthodes telles que `getHours()`, `getMinutes()` et `getSeconds()` pour comparer les heures, les minutes et les secondes respectivement. Sinon, les dates avec des heures, minutes ou secondes différentes seront considérées comme égales.

## Voir aussi

- [Documentation TypeScript sur l'objet Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-2.html#comparaison-de-postes-de-travail)
- [Article d'exploration des dates en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)