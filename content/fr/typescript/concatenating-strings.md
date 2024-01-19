---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

Concaténer des chaînes signifie simplement combiner deux ou plusieurs chaînes de caractères en une seule. Les programmeurs le font pour manipuler et former efficacement les textes selon leurs besoins.

## Comment faire :

En TypeScript, vous pouvez concaténer des chaînes de caractères en utilisant l'opérateur `+` ou la méthode `concat()`, ou même utiliser les templates de chaînes avec `${}`. Jetez un œil à ces exemples :

```TypeScript
// Utilisation de l'opérateur +
let str1 = "Bonjour, ";
let str2 = "monde !";
let res = str1 + str2;
console.log(res);  // "Bonjour, monde !"

// Utilisation de la méthode concat()
let res2 = str1.concat(str2);
console.log(res2);  // "Bonjour, monde !"

// Utilisation des templates de chaînes
let res3 = `${str1}${str2}`;
console.log(res3);  // "Bonjour, monde !"
```

## Plongée en profondeur :

Historiquement, l'opérateur `+` est employé dans les langages de programmation depuis les débuts pour concaténer des chaînes. Cependant, à mesure que les langages évoluent, des alternatives, comme la méthode `concat()` et les templates de chaînes, sont apparues pour offrir plus de flexibilité et de lisibilité.

En termes d'implémentation, l'opérateur `+` et la méthode `concat()` fonctionnent fondamentalement de la même manière. Ils créent une nouvelle chaîne tout en laissant les chaînes originales intactes. En revanche, les templates de chaînes, introduits avec ES6, permettent non seulement la concaténation, mais aussi l'interpolation de variables, rendant le code beaucoup plus lisible.

## Voir aussi :

TypeScript Documentation: [String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
Mozilla Developer Network: [String concatenation](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Text_formatting_with_%60_%60)
Ecma International: [Template Literals](https://www.ecma-international.org/ecma-262/6.0/#sec-template-literals)