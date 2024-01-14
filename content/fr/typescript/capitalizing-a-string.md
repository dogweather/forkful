---
title:    "TypeScript: Mettre en majuscule une chaîne de caractères"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est parfois nécessaire de convertir une chaîne de caractères en majuscules, que ce soit pour des raisons esthétiques ou pour des besoins spécifiques en programmation. Dans cet article, nous allons vous montrer comment capitaliser une chaîne de caractères en TypeScript.

## Comment faire

Tout d'abord, nous devons définir la chaîne de caractères que nous voulons capitaliser. Nous pouvons le faire en utilisant la syntaxe suivante :

```TypeScript
let chaineDeCaracteres = "exEmpLe";
```

Maintenant, pour capitaliser cette chaîne de caractères, nous allons utiliser la méthode `toUpperCase()`, qui convertit toutes les lettres de la chaîne en majuscules. Nous pouvons l'appliquer à notre chaîne définie précédemment de la manière suivante :

```TypeScript
let chaineDeCaracteres = "exEmPle";
let chaineEnMajuscules = chaineDeCaracteres.toUpperCase();
console.log(chaineEnMajuscules); // affichera "EXEMPLE" dans la console
```

De plus, si nous voulons capitaliser uniquement la première lettre d'une chaîne de caractères, nous pouvons utiliser la méthode `charAt()` pour cibler la première lettre et la méthode `toUpperCase()` pour la convertir en majuscule, comme ceci :

```TypeScript
let chaineDeCaracteres = "exEmPle";
let premiereLettre = chaineDeCaracteres.charAt(0).toUpperCase();
let resteDeLaChaine = chaineDeCaracteres.slice(1);
let chaineCapitalisee = premiereLettre + resteDeLaChaine;
console.log(chaineCapitalisee); // affichera "ExEmPle" dans la console
```

## Plongée en profondeur

Lorsque nous utilisons la méthode `toUpperCase()` pour capitaliser une chaîne de caractères, il est important de noter qu'elle ne modifie pas la chaîne originale, mais renvoie une nouvelle chaîne avec les modifications appliquées. De plus, il est important de garder à l'esprit que cette méthode ne prend pas en compte les accents ou les caractères spéciaux, elle ne convertit que les lettres en majuscules.

## Voir aussi

- [Documentation officielle de la méthode toUpperCase()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toUpperCase)
- [Autres méthodes utiles pour manipuler les chaînes de caractères en TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-operations)