---
title:    "TypeScript: Concaténation de chaînes"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi concaténer des chaînes de caractères en TypeScript

Les chaînes de caractères font partie des éléments les plus couramment utilisés en programmation. Que ce soit pour afficher une variable, un message à l'utilisateur ou manipuler des données, il est souvent nécessaire de concaténer des chaînes de caractères en TypeScript. Mais pourquoi devriez-vous le faire ?

## Comment le faire

Pour concaténer des chaînes de caractères en TypeScript, vous pouvez utiliser l'opérateur `+` ou la méthode `concat()`.
Voici un exemple de chaque méthode :
```TypeScript
let prenom: string = "Jean";
let nom: string = "Dupont";
let message: string = "Bonjour" + prenom + nom; //utilisation de l'opérateur +
console.log(message); //affichera "Bonjour Jean Dupont"

let message2: string = "Bonjour".concat(" ", prenom, " ", nom); //utilisation de la méthode concat()
console.log(message2); //affichera "Bonjour Jean Dupont"
```

Il est également possible d'utiliser des templates de chaînes en utilisant des backticks `` ` `` et des placeholders `${}`. Ce qui rend le code plus lisible et plus facile à maintenir.
````TypeScript
let age: number = 25;
let message3: string = `Bonjour ${prenom} ${nom}, tu as ${age} ans.`;
console.log(message3); //affichera "Bonjour Jean Dupont, tu as 25 ans."
````

## Approfondissement

Lorsque vous concaténez des chaînes de caractères, il est important de faire attention aux types des variables que vous utilisez. En effet, si vous concaténez une chaîne de caractères avec un nombre, celui-ci sera automatiquement converti en chaîne. Cela peut entraîner des erreurs si vous effectuez ensuite des opérations arithmétiques sur cette variable.

Il est également important de noter que les templates de chaînes en TypeScript offrent une meilleure performance que l'utilisation de l'opérateur `+` ou de la méthode `concat()`. Les templates de chaînes sont évalués à l'avance, tandis que l'opérateur `+` ou la méthode `concat()` doivent concaténer les chaînes à chaque exécution.

Enfin, TypeScript étant un langage à typage statique, vous devez faire attention à ce que les types des variables que vous concaténez soient compatibles. Par exemple, vous ne pouvez pas concaténer une chaîne de caractères avec un objet.

## Voir aussi

- [Documentation officielle de TypeScript sur les chaînes de caractères](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Article sur les templates de chaînes en TypeScript](https://blog.grossman.io/template-strings-typescript/)
- [Comparaison de performance entre les différents moyens de concaténer des chaînes en JavaScript](http://jsben.ch/EBAI3)