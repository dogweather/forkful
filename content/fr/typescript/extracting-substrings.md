---
title:    "TypeScript: Extraction de sous-chaînes"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi extraire des sous-chaînes?

L'extraction de sous-chaînes peut être utile dans de nombreux scénarios de programmation. Par exemple, si vous devez remplacer une partie spécifique d'une chaîne par une autre valeur ou si vous avez besoin de récupérer un numéro de téléphone à partir d'une chaîne de texte. En général, l'extraction de sous-chaînes est utilisée pour obtenir une partie spécifique d'une chaîne de caractères plutôt que la chaîne entière.

## Comment faire
Pour extraire une sous-chaîne en TypeScript, vous devez utiliser la méthode `substring()` en spécifiant l'indice de début et l'indice de fin de la sous-chaîne que vous souhaitez extraire. Par exemple:
```TypeScript
let texte: string = "Bonjour le monde";
let sousChaine: string = texte.substring(8, 11);
console.log(sousChaine); // outputs "le"
```
Dans cet exemple, nous avons extrait les caractères situés entre les indices 8 et 11, créant ainsi la sous-chaîne "le".

Vous pouvez également utiliser la méthode `slice()` pour extraire une sous-chaîne en spécifiant l'indice de début et éventuellement l'indice de fin. La différence entre `substring()` et `slice()` est que `slice()` accepte également des indices négatifs, ce qui peut être utile pour extraire une sous-chaîne à partir de la fin de la chaîne de caractères. Par exemple:
```TypeScript
let texte: string = "Bonjour le monde";
let sousChaine: string = texte.slice(8, -3);
console.log(sousChaine); // outputs "le mo"
```
Dans cet exemple, nous avons extrait une sous-chaîne commençant à l'indice 8 et se terminant à 3 caractères avant la fin de la chaîne.

Il est également possible d'utiliser des expressions régulières pour extraire une sous-chaîne à l'aide de la méthode `match()` ou `replace()`. Par exemple, pour récupérer un numéro de téléphone à partir d'une chaîne de texte:
```TypeScript
let texte: string = "Mon numéro de téléphone est le 123-456-7890.";
let numero: string = texte.match(/\d{3}-\d{3}-\d{4}/)[0];
console.log(numero); // outputs "123-456-7890"
```

## Plongez plus profondément
L'extraction de sous-chaînes peut être plus complexe en fonction de vos besoins. Par exemple, vous pouvez utiliser la méthode `substring()` pour extraire une sous-chaîne à partir d'une chaîne multi-lignes en spécifiant l'indice de début et l'indice de fin de ligne plutôt que d'utiliser des indices de caractères. Vous pouvez également utiliser la fonction `indexOf()` pour déterminer l'indice de début d'une sous-chaîne à extraire.

L'utilisation de méthodes telles que `trim()` ou `toLowerCase()` avant l'extraction d'une sous-chaîne peut également faciliter le processus en éliminant les espaces ou en normalisant la casse des caractères.

## Voir aussi
- [Documentation officielle TypeScript sur la méthode `substring()`](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [Documentation officielle TypeScript sur la méthode `slice()`](https://www.typescriptlang.org/docs/handbook/strings.html#slice)
- [Documentation officielle JavaScript sur les expressions régulières](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_r%C3%A9guli%C3%A8res)