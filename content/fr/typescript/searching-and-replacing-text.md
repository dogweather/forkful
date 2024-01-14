---
title:                "TypeScript: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Les développeurs utilisent souvent la recherche et le remplacement de texte lors de l'écriture de code TypeScript. Cela leur permet de trouver rapidement et facilement des parties spécifiques de leur code et de les remplacer par une nouvelle valeur. Cela peut être utile pour corriger des erreurs, mettre à jour des noms de variables ou de fonctions, ou encore pour effectuer des modifications en masse sur un grand nombre de lignes de code.

## Comment faire

Voici un exemple simple de recherche et de remplacement de texte en TypeScript :

```TypeScript
let message: string = "Bonjour, je suis un développeur TypeScript.";

// Remplacer "développeur" par "codeur"
message = message.replace("développeur", "codeur");

console.log(message);
// Output: "Bonjour, je suis un codeur TypeScript."
```

Dans cet exemple, nous utilisons la méthode `replace` de l'objet `string` pour remplacer le mot "développeur" par "codeur" dans la variable `message`. La nouvelle valeur est ensuite imprimée dans la console.

On peut également utiliser des expressions régulières pour effectuer des recherches et remplacements plus complexes. Voici un exemple utilisant l'expression régulière `/\d+/g` pour remplacer tous les nombres de la chaîne par des astérisques :

```TypeScript
let message: string = "J'ai 10 pommes et 5 bananes.";

// Remplacer tous les nombres par des astérisques
message = message.replace(/\d+/g, "*");

console.log(message);
// Output: "J'ai * pommes et * bananes."
```

## Plongée en profondeur

Il est important de noter que la méthode `replace` ne modifie pas la chaîne originale, mais renvoie une nouvelle chaîne avec les modifications apportées. Pour modifier la chaîne originale, il faut attribuer la valeur de retour à la variable d'origine.

De plus, la méthode `replace` ne modifie que la première occurrence de la valeur recherchée. Pour remplacer toutes les occurrences, il faut utiliser une expression régulière avec le modificateur `g` (global).

Il existe également d'autres méthodes de l'objet `string` qui peuvent être utilisées pour rechercher et modifier du texte, telles que `indexOf`, `lastIndexOf`, `slice` et `substring`.

## Voir aussi

- [Documentation officielle TypeScript : Méthode replace](https://www.typescriptlang.org/docs/handbook/strings.html#string-replace)
- [Documentation officielle JavaScript : Méthode replace](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Guide des expressions régulières en JavaScript](https://openclassrooms.com/courses/utiliser-les-expressions-regulieres-en-javascript)