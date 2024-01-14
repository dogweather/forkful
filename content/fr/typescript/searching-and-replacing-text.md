---
title:                "TypeScript: Recherche et remplacement de texte"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation, qui peuvent être effectuées pour diverses raisons telles que la correction de fautes d'orthographe, la mise à jour de noms de variables ou la modification de chaînes de caractères. Cela peut vous faire économiser beaucoup de temps et d'efforts, surtout si vous avez un grand nombre de lignes de code à modifier.

## Comment faire

La recherche et le remplacement de texte en TypeScript peuvent être réalisés en utilisant la méthode `replace` de l'objet `String`. Voici un exemple de code pour rechercher et remplacer un mot spécifique dans une chaîne de caractères :

```TypeScript
let phrase: string = "Bonjour à tous !";
let nouvellePhrase: string = phrase.replace("Bonjour", "Hello");
console.log(nouvellePhrase);

// Sortie : "Hello à tous !"
```

Vous pouvez également utiliser des expressions régulières pour effectuer des recherches et des remplacements plus complexes. Voici un exemple pour remplacer toutes les occurrences de chiffres dans une chaîne de caractères par des astérisques :

```TypeScript
let phrase: string = "Mon code secret est 12345.";
let nouvellePhrase: string = phrase.replace(/\d/g, "*");
console.log(nouvellePhrase);

// Sortie : "Mon code secret est *****."
```

## Plongée en profondeur

En TypeScript, la méthode `replace` prend deux paramètres : la chaîne à rechercher et la chaîne de remplacement. Cependant, en utilisant des expressions régulières, vous pouvez également utiliser des groupes de capture pour capturer des parties spécifiques de la chaîne d'origine et les utiliser dans la chaîne de remplacement. Par exemple :

```TypeScript
let html: string = "<h1>Bienvenue sur mon site !</h1>";
let nouvelleHtml: string = html.replace(/<([\/]?)h1>/g, "<$1h2>");
console.log(nouvelleHtml);

// Sortie : "<h2>Bienvenue sur mon site !</h2>"
```

En utilisant des expressions régulières, les possibilités de recherche et de remplacement de texte sont presque infinies. N'hésitez pas à explorer et à expérimenter pour trouver la meilleure méthode pour votre cas d'utilisation.

## Voir aussi

- La documentation officielle de TypeScript sur la méthode `replace` : https://www.typescriptlang.org/docs/handbook/basic-types.html#string-replace
- Un tutoriel sur les expressions régulières en TypeScript : https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm
- Un outil en ligne pour tester et créer des expressions régulières : https://regex101.com/