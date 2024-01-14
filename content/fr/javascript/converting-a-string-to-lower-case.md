---
title:                "Javascript: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi 

Convertir une chaîne de caractères en lettres minuscules est une tâche courante lors de la programmation en JavaScript. Cela permet notamment de s'assurer que les valeurs comparées sont uniformisées et que les données sont correctement formatées. Dans cet article, nous allons expliquer comment effectuer cette conversion de manière simple et efficace.

## Comment faire

Il existe plusieurs façons de convertir une chaîne de caractères en lettres minuscules en JavaScript. Voici deux méthodes simples qui peuvent être utilisées selon les besoins :

```JavaScript
// Exemple 1 : utilisation de la méthode toLowerCase()
let str = "Bonjour le Monde!"
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr); // Affiche "bonjour le monde!"

// Exemple 2 : utilisation de l'opérateur de décomposition et de la méthode toLowerCase()
let str = "Bonjour le Monde!"
let lowerCaseStr = [...str].map(char => char.toLowerCase()).join("");
console.log(lowerCaseStr); // Affiche "bonjour le monde!"
```

Dans le premier exemple, nous utilisons la méthode `toLowerCase()` qui est disponible sur les chaînes de caractères en JavaScript. Elle prend la chaîne en paramètre et renvoie une nouvelle chaîne avec toutes les lettres en minuscules.

Dans le deuxième exemple, nous utilisons une technique appelée opérateur de décomposition pour séparer la chaîne en un tableau de caractères. Puis, grâce à la méthode `map()`, nous itérons à travers chaque caractère et les convertissons en minuscules avec la méthode `toLowerCase()`. Finalement, nous utilisons la méthode `join()` pour rassembler les caractères en une seule chaîne.

## Deep Dive 

La méthode `toLowerCase()` utilise les règles de l'Alphabet Unicode pour effectuer la conversion en minuscules. Cela peut entraîner des différences entre les résultats en fonction de la langue utilisée. De plus, cette méthode ne prend pas en compte les caractères spéciaux, accents ou cas particuliers comme les lettres majuscules accentuées en français. C'est pourquoi lors de la manipulation de chaînes de caractères contenant des lettres spéciales, il est recommandé d'utiliser une bibliothèque externe comme `lodash` ou `string.js` qui fournissent des fonctions de conversion plus complètes.

## Voir aussi

- [Documentation de la méthode toLowerCase()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase)
- [Opérateur de décomposition en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Opérateurs/Décomposition)
- [Bibliothèque lodash](https://lodash.com)
- [Bibliothèque string.js](https://stringjs.com)