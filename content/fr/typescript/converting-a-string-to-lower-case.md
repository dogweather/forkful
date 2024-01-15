---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "TypeScript: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà retrouvé avec une chaîne de caractères en majuscules alors que vous auriez préféré l'avoir en minuscules ? Peut-être que vous avez besoin de comparer deux chaînes de caractères sans tenir compte de la casse ou peut-être que vous voulez simplement que votre texte ait un aspect plus cohérent. Dans ces cas-là, il est utile de savoir comment convertir une chaîne de caractères en minuscules en utilisant TypeScript.

## Comment faire

```TypeScript
let string = "HELLO WORLD";
console.log(string.toLowerCase()); // output: "hello world"
```

Le code ci-dessus montre comment utiliser la méthode `toLowerCase()` pour convertir une chaîne de caractères en minuscules. Il suffit d'utiliser cette méthode sur la chaîne de caractères que vous souhaitez convertir et le tour est joué !

## Plongée en profondeur

La méthode `toLowerCase()` fait partie des méthodes de manipulation de chaînes de caractères disponibles en TypeScript. Elle est également disponible dans d'autres langages de programmation tels que JavaScript et Java. Cette méthode est utilisée pour retourner une copie de la chaîne de caractères originale avec toutes les lettres en minuscules.

Il est important de noter que la méthode `toLowerCase()` ne modifie pas la chaîne de caractères originale mais renvoie plutôt une nouvelle chaîne de caractères avec les modifications apportées. Cela signifie que vous devez attribuer la valeur renvoyée par la méthode à une variable si vous souhaitez conserver la chaîne de caractères en minuscules.

Par exemple :

```TypeScript
let string = "HELLO WORLD";
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString); // output: "hello world"
console.log(string); // output: "HELLO WORLD"
```

En plus de transformer les lettres en minuscules, la méthode `toLowerCase()` supprime également tous les accents et les signes de ponctuation de la chaîne de caractères. Cela peut être utile si vous avez besoin de comparer des chaînes de caractères sans tenir compte des accents ou des signes de ponctuation.

## Voir aussi

- [Documentation officielle de TypeScript sur les méthodes de manipulation de chaînes de caractères](https://www.typescriptlang.org/docs/handbook/strings.html#string-methods)
- [Article sur la méthode `toLowerCase()` en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase)