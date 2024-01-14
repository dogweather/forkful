---
title:    "TypeScript: Mettre en majuscule une chaîne de caractères"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes lors de la manipulation de données en programmation est de capitaliser une chaîne de caractères. Cela peut sembler simple, mais cela peut avoir un grand impact sur l'apparence et la lisibilité de votre code final. En utilisant TypeScript, nous pouvons facilement capitaliser une chaîne de caractères en utilisant les outils et les fonctions intégrés.

## Comment Faire

Pour capitaliser une chaîne de caractères en TypeScript, nous pouvons utiliser la méthode `toUpperCase()` intégrée à l'objet String. Cette méthode prend chaque lettre de la chaîne de caractères et la convertit en majuscule.

Voici un exemple de code pour capitaliser une chaîne de caractères en TypeScript :

```TypeScript
let myString = "bonjour le monde";

// Utilisation de la méthode toUpperCase() pour capitaliser la chaîne de caractères
let capitalizedString = myString.toUpperCase();

console.log(capitalizedString); // "BONJOUR LE MONDE"
```

Dans cet exemple, nous avons déclaré une variable `myString` contenant la chaîne de caractères que nous voulons capitaliser. Ensuite, nous avons utilisé la méthode `toUpperCase()` pour créer une nouvelle variable `capitalizedString` qui contiendra la version capitalisée de `myString`. Enfin, nous avons utilisé `console.log()` pour afficher le résultat à l'écran.

Nous pouvons également capitaliser uniquement la première lettre d'une chaîne de caractères en utilisant la méthode `charAt()` pour accéder à la première lettre et la méthode `toUpperCase()` pour la convertir en majuscule. Voici un exemple de code pour cela :

```TypeScript
let myString = "hello world";

// Utilisation de la méthode toUpperCase() pour capitaliser uniquement la première lettre
let capitalizedString = myString.charAt(0).toUpperCase() + myString.slice(1);

console.log(capitalizedString); // "Hello world"
```

Dans cet exemple, nous avons utilisé la méthode `charAt()` pour accéder à la première lettre de `myString` et ensuite nous avons utilisé la méthode `toUpperCase()` pour la convertir en majuscule. Ensuite, nous avons utilisé la méthode `slice()` pour récupérer le reste de la chaîne de caractères à partir du deuxième caractère. Enfin, nous avons concaténé la première lettre capitalisée avec le reste de la chaîne de caractères pour obtenir notre chaîne de caractères capitalisée.

## Plongée en Profondeur

Il est important de noter que ces méthodes de capitalisation en TypeScript sont sensibles à la casse. Cela signifie que si nous avons une chaîne de caractères avec des lettres majuscules et minuscules mélangées, la méthode `toUpperCase()` va simplement convertir toutes les lettres en majuscules, tandis que la méthode `charAt()` ne capitalisera que la première lettre.

De plus, ces méthodes ne fonctionnent que pour les caractères anglais de base. Si nous utilisons des caractères accentués ou des caractères d'autres langues, nous devrons utiliser d'autres méthodes ou fonctions de manipulation de chaînes de caractères.

## Voir Aussi

Pour en savoir plus sur les méthodes de manipulation de chaînes de caractères en TypeScript, consultez la [documentation officielle de TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html) ainsi que les guides et tutoriels en ligne disponibles.