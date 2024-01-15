---
title:                "Rechercher et remplacer du texte"
html_title:           "TypeScript: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

On a tous eu à un moment donné à faire des recherches et remplacements de textes dans nos fichiers de code. Que ce soit pour corriger des erreurs ou pour effectuer des modifications massives, c'est une tâche très courante pour les développeurs. Heureusement, TypeScript offre des fonctionnalités pratiques pour faciliter cette tâche fastidieuse.

## Comment faire

Voici comment utiliser les fonctionnalités de recherche et de remplacement de textes en TypeScript :

```typescript
var myString = "Bonjour le monde";

// Recherche et remplacement d'un mot spécifique
var newString = myString.replace("Bonjour", "Salut");

console.log(newString); // Output: Salut le monde


// Recherche et remplacement avec une expression régulière
var newString2 = myString.replace(/bonjour/gi, "hello");

console.log(newString2); // Output: hello le monde
```

## Plongée en profondeur

La méthode `replace()` en TypeScript peut prendre deux paramètres : le texte à rechercher et le texte de remplacement. Mais elle peut également accepter une expression régulière en tant que premier paramètre. Cela permet d'effectuer des recherches et remplacements plus complexes en utilisant des modèles de texte.

En utilisant des indicateurs tels que `g` (global) et `i` (insensitive), vous pouvez rechercher et remplacer tous les occurrences du motif dans une chaîne de caractères, qu'ils soient en majuscule ou en minuscule. Cela peut être très utile lorsqu'on a besoin de modifier plusieurs lignes de code en même temps.

## Voir aussi

- [Documentation officielle de la méthode `replace()` en TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#replace)
- [Guide complet sur les expressions régulières en TypeScript](https://www.regular-expressions.info/typescript.html)
- [Exemples pratiques d'utilisation de `replace()` en TypeScript](https://stackabuse.com/search-and-replace-in-javascript-with-regular-expressions/)