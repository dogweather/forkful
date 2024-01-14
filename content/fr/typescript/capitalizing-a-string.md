---
title:                "TypeScript: Majusculation d'une chaîne de caractères"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La mise en majuscule d'une chaîne de caractères peut être utile pour améliorer la lisibilité ou pour afficher certains mots en évidence dans une application ou un site web. Elle peut également être nécessaire lors de la manipulation de données pour s'assurer qu'elles sont toutes uniformes.

## Comment faire

Il existe plusieurs façons de mettre en majuscule une chaîne de caractères en TypeScript. Voici deux exemples :

```TypeScript
// Utilisation de la méthode toUpperCase()
const chaine = "bonjour";
const chaineMaj = chaine.toUpperCase(); // BONJOUR
```

```TypeScript
// Utilisation de l'opérateur de décomposition et de la méthode toUpperCase()
const chaine = "bonjour";
const [premiereLettre, ...autresLettres] = chaine;
const chaineMaj = premiereLettre.toUpperCase() + autresLettres.join(""); // Bonjour
```

Dans les deux cas, la chaîne "bonjour" est mise en majuscule et stockée dans la variable `chaineMaj`. Si vous exécutez ces exemples, vous verrez le résultat suivant : `BONJOUR` et `Bonjour`.

## Plongée en profondeur

En TypeScript, les chaînes de caractères sont de type `string` et, par conséquent, possèdent des méthodes qui leur sont propres. La méthode `toUpperCase()` est l'une de ces méthodes et elle est utilisée pour mettre en majuscule tous les caractères d'une chaîne de caractères. Elle ne modifie pas la chaîne d'origine, mais renvoie une nouvelle chaîne avec les modifications. 

Il existe également d'autres méthodes utiles pour manipuler les chaînes de caractères en TypeScript, telles que `toLowerCase()`, `charAt()`, `trim()` et bien d'autres encore.

## Voir aussi
- [Documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
- [Tutoriel TypeScript pour débutants](https://developer.mozilla.org/fr/docs/Learn/Tools_and_testing/Client-side_JavaScript_frameworks_TypeScript/Debutant_%C3%A0_TypeScript)
- [Projet GitHub pour des exemples de code en TypeScript](https://github.com/microsoft/TypeScript-Node-Starter)