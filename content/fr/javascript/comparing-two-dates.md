---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

Comparer deux dates c'est, comme son nom l'indique, une comparaison de deux dates différentes pour déterminer laquelle est antérieure, ultérieure ou si elles sont les mêmes. Les programmeurs le font habituellement pour manipuler des informations basées sur le temps dans leurs applications.

## Comment faire :

```Javascript
let date1 = new Date("2025-01-01");
let date2 = new Date("2025-12-31");

if(date1 > date2) {
  console.log("La date1 est ultérieure à la date2.");
} else if(date1 < date2) {
  console.log("La date1 est antérieure à la date2.");
} else {
  console.log("Les deux dates sont égales.");
}
```
Dans cet exemple, la console affichera "La date1 est antérieure à la date2.".

## Deep Dive

Historiquement, JavaScript n'offrait pas d'opération directe pour comparer des dates. Dans les versions précédentes, les utilisateurs devaient convertir manuellement les dates en timbres millisecondes avant de pouvoir les comparer. Cependant, l'opérateur de comparaison de JavaScript fonctionne maintenant directement avec les objets Date.

Alternativement, des bibliotheques comme moment.js permettent une comparaison plus complexe et flexible des dates. Elles sont particulièrement utiles lorsque vous travaillez avec des fuseaux horaires différents.

Notez que lors de l'utilisation de la comparaison, JavaScript convertit automatiquement l'objet Date en millisecondes depuis l'époque Unix (1er janvier 1970), puis effectue la comparaison. 

## Voir aussi

- MDN Web Docs sur l'objet Date : [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Librairie moment.js pour gérer les dates : [https://momentjs.com/](https://momentjs.com/)
- Date-fns, une alternative moderne à moment.js : [https://date-fns.org/](https://date-fns.org/)