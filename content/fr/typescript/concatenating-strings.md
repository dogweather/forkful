---
title:                "Concaténer des chaînes de caractères"
html_title:           "TypeScript: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Concaténer des chaînes de caractères est un moyen pour les programmeurs de combiner plusieurs chaînes de caractères en une seule. Cela peut être utile lorsque nous voulons créer du texte dynamique ou combiner des données en une seule chaîne. C'est une fonctionnalité courante dans de nombreux langages de programmation.

## Comment faire:

```TypeScript
// Exemple 1: Concaténer deux chaînes de caractères
let firstName: string = "Jean";
let lastName: string = "Dupont";
let fullName: string = `${firstName} ${lastName}`;
console.log(fullName);
// Sortie: Jean Dupont

// Exemple 2: Concaténer une chaîne de caractères avec un nombre
let productName: string = "Ordinateur portable";
let quantity: number = 3;
let order: string = `Vous avez commandé ${quantity} ${productName}s`;
console.log(order);
// Sortie: Vous avez commandé 3 Ordinateur portables
```

## Approfondissement:

La concaténation de chaînes de caractères est une pratique courante depuis les débuts de la programmation. Avant l'utilisation de modèles de chaînes (comme dans notre exemple ci-dessus), les programmeurs utilisaient des méthodes telles que `String.concat()` pour combiner des chaînes de caractères.

Bien que la concaténation de chaînes de caractères soit une méthode courante, elle peut être coûteuse en termes de performances pour les grands ensembles de données. Dans ces cas, il peut être préférable d'utiliser la méthode `String.join()` ou de créer un tableau de chaînes de caractères et d'utiliser la méthode `Array.join()` pour les concaténer.

## Voir aussi:

- [La documentation officielle de TypeScript sur les chaînes de caractères](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Un article sur les performances de la concaténation de chaînes de caractères en JavaScript](https://ariya.io/2013/03/performance-of-javascript-string-concatenation)
- [La spécification ECMAScript sur la concaténation de chaînes de caractères](https://www.ecma-international.org/ecma-262/6.0/#sec-string-concatenation)