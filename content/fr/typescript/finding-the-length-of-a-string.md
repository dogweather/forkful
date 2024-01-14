---
title:    "TypeScript: Trouver la longueur d'une chaîne de caractères."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation. Cela peut être utile pour effectuer des validations, découper une chaîne en plusieurs parties ou simplement pour des besoins d'affichage. Dans cet article, nous allons apprendre comment trouver facilement la longueur d'une chaîne en utilisant TypeScript.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en TypeScript, nous pouvons utiliser la méthode `length` sur la chaîne elle-même. Cette méthode renvoie un nombre qui représente le nombre de caractères dans la chaîne.

Voici un exemple de code pour trouver la longueur d'une chaîne :

```TypeScript
let str: string = "Bonjour";
console.log(str.length);
```

Nous déclarons tout d'abord une variable `str` contenant la chaîne "Bonjour". Ensuite, nous affichons la longueur de cette chaîne en utilisant la méthode `length`. Dans ce cas, le résultat sera 7 car il y a 7 caractères dans la chaîne "Bonjour".

Ci-dessous, vous trouverez d'autres exemples de code avec différents types de chaînes de caractères :

```TypeScript
let str1: string = "Ceci est une chaîne avec des mots.";
console.log(str1.length); // affichera 33

let str2: string = "12345";
console.log(str2.length); // affichera 5

let str3: string = "";
console.log(str3.length); // affichera 0
```

Comme vous pouvez le voir, la méthode `length` fonctionne avec toutes sortes de chaînes de caractères, qu'elles soient longues, courtes ou même vides.

## Plongée en profondeur

En utilisant la méthode `length`, nous pouvons également effectuer des validations pour s'assurer qu'une chaîne a une longueur spécifique. Par exemple, si nous voulons vérifier qu'une adresse email a une longueur minimale de 10 caractères, nous pouvons simplement utiliser une condition `if` avec la méthode `length` pour vérifier cela.

Il est également important de noter que la méthode `length` compte les espaces dans une chaîne comme des caractères. Donc, si vous avez besoin de compter uniquement les caractères alphabétiques ou numériques, vous devrez utiliser d'autres méthodes comme `split` et `filter`.

## Voir aussi

- [Documentation sur les chaînes de caractères en TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Autres méthodes utiles pour les chaînes de caractères en TypeScript](https://www.javatpoint.com/typescript-string-functions)