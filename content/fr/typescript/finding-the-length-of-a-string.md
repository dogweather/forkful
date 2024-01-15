---
title:                "Trouver la longueur d'une chaîne"
html_title:           "TypeScript: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères peut sembler une tâche simple et banale pour certains, mais c'est en réalité une compétence clé en programmation TypeScript. Connaître la longueur d'une chaîne peut être utile dans de nombreux cas, par exemple pour valider les entrées utilisateurs, manipuler des données ou créer des logiques conditionnelles. En outre, c'est une compétence fondamentale nécessaire pour comprendre de nombreux autres concepts en programmation.

## Comment faire

Pour trouver la longueur d'une chaîne en TypeScript, nous pouvons utiliser la méthode `length` qui est disponible sur les types de données de type `string`. Voici un exemple simple :

```
let texte: string = "Bonjour le monde!";
console.log(texte.length); // Output: 17
```

Nous pouvons également utiliser la propriété `length` directement sur un littéral de chaîne de caractères, sans avoir à déclarer une variable :

```
console.log("Salut!".length); // Output: 6
```

Il est important de noter que cette méthode ne compte que les caractères dans la chaîne, les espaces comptent également. Si vous souhaitez exclure les espaces ou les caractères spéciaux, vous devrez utiliser une logique supplémentaire pour les supprimer avant de mesurer la longueur de la chaîne.

## Plongée en profondeur

En TypeScript, comme dans de nombreux autres langages de programmation, chaque caractère dans une chaîne est en réalité représenté par un numéro appelé "index". Ces index commencent à compter à partir de zéro pour le premier caractère, et augmentent d'un pour chaque caractère suivant. Prenons l'exemple suivant pour mieux comprendre :

```
let texte: string = "Typescript est amusant!";
console.log(texte[0]); // Output: T
console.log(texte[10]); // Output:  a
console.log(texte[18]); // Output: !
```

Nous pouvons également utiliser ces index pour parcourir une chaîne de caractères en utilisant des boucles. Par exemple, nous pouvons utiliser une boucle `for` pour afficher chaque caractère de la chaîne:

```
let texte: string = "Hello World!";
for (let i = 0; i < texte.length; i++) {
  console.log(texte[i]);
}
// Output: H
//         e
//         l
//         l
//         o
// 
//         W
//         o
//         r
//         l
//         d
//         !
```

Enfin, il est également utile de savoir que la méthode `length` ne fonctionne que sur les chaînes de caractères. Si vous essayez de l'utiliser sur d'autres types de données, vous obtiendrez une erreur. Il est donc important de comprendre les types de données avec lesquels vous travaillez lorsque vous utilisez cette méthode.

## Voir aussi

- [Documentation officielle TypeScript](https://www.typescriptlang.org/docs)
- [Guide de démarrage rapide TypeScript](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Tutoriels TypeScript sur YouTube](https://www.youtube.com/playlist?list=PLC3y8-rFHvwhgWwm5J3KqzX47n7dwWNrq)