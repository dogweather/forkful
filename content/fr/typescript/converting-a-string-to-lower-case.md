---
title:                "TypeScript: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de convertir une chaîne de caractères en minuscules dans le cadre du développement de logiciels. Cela peut être utile pour comparer des chaînes de caractères de manière insensible à la casse ou pour simplement rendre une chaîne plus lisible pour l'utilisateur.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en TypeScript, il suffit d'utiliser la méthode `toLowerCase()` sur la chaîne en question. Voici un exemple de code et sa sortie :

```TypeScript
let chaine = "Bonjour le Monde!";
console.log(chaine.toLowerCase()); // affiche "bonjour le monde!"
```

Comme vous pouvez le voir, la méthode `toLowerCase()` a converti toutes les lettres en minuscules.

## Approfondissement

En TypeScript, la méthode `toLowerCase()` utilise les règles de conversion Unicode pour déterminer quelle lettre doit être mise en minuscule. Cela signifie que la conversion peut différer en fonction de la langue ou de l'alphabet utilisé. Par exemple, la lettre "İ" en turc sera convertie en "i" en minuscule, alors qu'en anglais, elle sera convertie en "i̇".

De plus, il est important de noter que la méthode `toLowerCase()` ne modifie pas la chaîne originale, mais retourne une nouvelle chaîne convertie. Si vous souhaitez modifier la chaîne originale, il faudra la réaffecter à la variable d'origine.

## Voir aussi

- [Documentation officielle TypeScript pour la méthode `toLowerCase()`](https://www.typescriptlang.org/docs/handbook/strings.html#string-operations)
- [Article sur les règles de conversion Unicode pour la méthode `toLowerCase()`](https://unicode.org/reports/tr21/#Case_Folding)
- [Article sur les différences de traitement de la casse dans différentes langues](https://en.wikipedia.org/wiki/Letter_case#Language-specific_differences)