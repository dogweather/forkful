---
title:                "Majuscules d'une chaîne"
html_title:           "TypeScript: Majuscules d'une chaîne"
simple_title:         "Majuscules d'une chaîne"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Capitaliser une chaîne de caractères en programmation signifie mettre la première lettre en majuscule et les autres en minuscules. Les programmeurs le font souvent lorsqu'ils doivent traiter des données utilisateur afin de les rendre plus cohérentes ou de les utiliser dans des conditions de recherche.

## Comment faire:
Voici un exemple simple en TypeScript pour capitaliser une chaîne de caractères donnée:

```TypeScript
let myString = "ceci est une phrase";
let capitalizedString = myString[0].toUpperCase() + myString.slice(1).toLowerCase();
console.log(capitalizedString);

// Output: "Ceci est une phrase"
```

## Plongeon en profondeur:
- Historique: Le concept de capitaliser une chaîne de caractères existe depuis longtemps et a été utilisé dans les anciens langages de programmation tels que COBOL.
- Alternatives: Il existe d'autres méthodes pour capitaliser une chaîne de caractères telles que l'utilisation de fonctions natives comme `toUpperCase()` ou des packages tiers.
- Détails de mise en œuvre: La méthode présentée dans la section How to utilise la propriété string `slice()` pour extraire la partie de la chaîne après la première lettre et la concatène avec la première lettre en majuscule. Cela fonctionne car les chaînes de caractères sont des tableaux de caractères en JavaScript.

## Voir aussi:
- Documentation TypeScript sur les chaînes de caractères : https://www.typescriptlang.org/docs/handbook/strings.html
- Un package JavaScript pour capitaliser les chaînes de caractères : https://www.npmjs.com/package/capitalize