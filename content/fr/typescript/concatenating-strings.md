---
title:                "TypeScript: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes est une pratique courante en programmation TypeScript. Elle permet de combiner plusieurs chaînes de caractères pour former une seule et unique chaîne. Cela peut être utile dans différentes situations, notamment pour la création de messages d'erreur, la manipulation de données ou encore la génération de contenu dynamique pour les applications web.

## Comment faire

La concaténation de chaînes peut se faire de différentes manières en TypeScript, mais la plus simple consiste à utiliser l'opérateur "+" entre deux chaînes de caractères. Voici un exemple de code :

```TypeScript
let prenom: string = "Jean";
let nom: string = "Dupont";
let phrase: string = prenom + " " + nom;

console.log(phrase);

// Output : "Jean Dupont"
```

On peut également utiliser la méthode "concat" pour concaténer plusieurs chaînes :

```TypeScript
let prenom: string = "Jean";
let nom: string = "Dupont";
let phrase: string = prenom.concat(" ", nom);

console.log(phrase);

// Output : "Jean Dupont"
```

Il est également possible de concaténer plusieurs chaînes dans une boucle pour créer une chaîne plus complexe :

```TypeScript
let fruits: string[] = ["pomme", "banane", "orange"];
let phrase: string = "Je mange des ";

for (let fruit of fruits) {
  phrase = phrase.concat(fruit, ", ");
}

console.log(phrase);

// Output : "Je mange des pommes, bananes, oranges, "
```

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes peut être gourmande en termes de performance, surtout si elle est utilisée de manière intensive dans une application. Cela est dû au fait que de nouvelles chaînes doivent être allouées en mémoire à chaque opération de concaténation.

Il existe également des librairies en TypeScript, telles que "lodash", qui proposent des fonctions de concaténation plus performantes, notamment avec l'utilisation de tableaux.

Il est donc important de prendre en compte l'impact sur les performances lors de l'utilisation de la concaténation de chaînes et de privilégier des alternatives plus optimisées si nécessaire.

## Voir aussi

- [Documentation officielle TypeScript sur la concaténation de chaînes](https://www.typescriptlang.org/docs/handbook/2/strings.html#string-concatenation)
- [Article sur les meilleures pratiques en matière de concaténation de chaînes en TypeScript](https://dzone.com/articles/string-interpolation-in-typescript)
- [Librairie "lodash" pour la manipulation de chaînes en TypeScript](https://lodash.com/docs/4.17.15#join)