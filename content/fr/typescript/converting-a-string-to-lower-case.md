---
title:                "TypeScript: Conversion d'une chaîne en minuscules"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion de chaîne de caractères en minuscules peut être utile lors de la manipulation de données dans un programme TypeScript. Cela peut faciliter la recherche et la comparaison de chaînes de caractères sans tenir compte de la casse.

## Comment faire

Il existe plusieurs façons de convertir une chaîne de caractères en minuscules en TypeScript. Voici quelques exemples :

```TypeScript
let texte = "Bonjour le Monde!";
console.log(texte.toLowerCase());
// sortie : bonjour le monde!
```

```TypeScript
let mot = "EXEMPLE";
console.log(mot.toLocaleLowerCase());
// sortie : exemple
```

```TypeScript
let phrase = "ceci est un EXEMPLE";
console.log(phrase.toLowerCase());
// sortie : ceci est un exemple
```

## Plongée plus profonde

La méthode `toLowerCase()` est une fonction intégrée en TypeScript qui transforme une chaîne de caractères en minuscules. Elle utilise les règles de mise en minuscules de la locale actuelle de l'utilisateur, ce qui peut varier en fonction de la langue et de la région.

Il est important de noter que la méthode `toLowerCase()` ne modifie pas la chaîne de caractères d'origine, mais renvoie plutôt une nouvelle chaîne de caractères avec la conversion en minuscules. Ainsi, si vous souhaitez utiliser la chaîne de caractères en minuscules, vous devrez la stocker dans une nouvelle variable.

De plus, la méthode `toLowerCase()` ne fonctionne que sur les caractères de l'alphabet. Les symboles et caractères spéciaux resteront inchangés.

## Voir aussi

- [Documentation sur la méthode toLowerCase() de TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#lowercasing)
- [Autres méthodes de manipulation de chaînes de caractères en TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#string-manipulation-using-string-methods)