---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitaliser une chaîne consiste à convertir la première lettre en majuscule. Les développeurs font ça pour standardiser les données et améliorer l'affichage des textes.

## How to:

Voici un exemple de capitalisation d'une chaîne de caractères en TypeScript:

```typescript
function capitalizeFirstLetter(text: string): string {
  return text.charAt(0).toUpperCase() + text.slice(1);
}

const myString = 'bonjour monde';
console.log(capitalizeFirstLetter(myString)); // Bonjour monde
```

## Deep Dive:

Historiquement, capitaliser un texte est un concept ancien, utilisé pour mettre en avant des noms propres et débuter des phrases. En programmation, la capitalisation permet de normaliser les saisies des utilisateurs, surtout pour les noms propres ou les titres.

Des alternatives existent : on pourrait utiliser des librairies comme `lodash` avec sa fonction `_.capitalize`, ou bien étendre le prototype `String` (mais c'est généralement une mauvaise pratique qui peut mener à des conflits).

Du point de vue de l'implémentation, notez que `charAt(0)` et `slice(1)` ne bégayent pas avec les chaînes multibytes (comme les emojis ou certains caractères spéciaux). Il est important de tester ces cas-là pour s'assurer que la fonction agit comme on le souhaite.

## See Also:

- MDN Web Docs sur `toUpperCase()`: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- Lodash library pour des opérations sur les chaînes de caractères: [https://lodash.com/docs/4.17.15#capitalize](https://lodash.com/docs/4.17.15#capitalize)
