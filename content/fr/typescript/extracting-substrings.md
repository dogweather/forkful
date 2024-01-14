---
title:                "TypeScript: Extraction de sous-chaînes"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi 

Extraires des sous-chaines de caractères est une tâche courante en programmation TypeScript. Cela peut être utile pour rechercher ou manipuler des données spécifiques dans une chaîne de caractères plus grande. Dans cet article, nous allons plonger dans les détails de l'extraction des sous-chaines en TypeScript et voir pourquoi cela peut être nécessaire dans votre code.

## Comment faire 

La méthode la plus courante pour extraire une sous-chaine en TypeScript est d'utiliser la fonction `substring()`. Voici un exemple :

```TypeScript
let chaine = "Bonjour tout le monde";
let sousChaine = chaine.substring(8, 14);
console.log(sousChaine);
```

Ceci va extraire la sous-chaine "tout le" de la chaine initiale et l'afficher dans la console. L'index de départ est 8 car les indices commencent à 0, et la longueur de la sous-chaine est de 6 caractères (14-8=6). Vous pouvez également spécifier un seul argument à `substring()` pour extraire tous les caractères à partir de cet index jusqu'à la fin de la chaine.

Une autre méthode pour extraire des sous-chaines est d'utiliser les bracket notation. Par exemple, si nous voulons extraire "hello" de la chaine "hello world", nous pouvons utiliser :

```TypeScript
let chaine = "hello world";
let sousChaine = chaine[0] + chaine[1] + chaine[2] + chaine[3] + chaine[4]; // ou simplement chaine[0,4]
console.log(sousChaine);
```

Cela va afficher "hello" dans la console. Vous pouvez également utiliser la fonction `slice()` pour extraire une sous-chaine en spécifiant l'index de départ et éventuellement l'index de fin.

## Plongée en profondeur 

La méthode `substring()` est préférée pour extraire des sous-chaines en TypeScript car elle est plus sûre en termes d'indices et de longueur de sous-chaine. L'utilisation de bracket notation peut être risquée car si l'index de fin est plus élevé que l'index de départ, cela peut entraîner une erreur. De plus, la fonction `slice()` peut retourner une sous-chaine vide si l'index de fin est inférieur à l'index de départ.

Il est également important de noter que les sous-chaines extraites sont des chaînes de caractères entièrement nouvelles et ne sont pas liées à la chaine d'origine. Cela signifie que si vous modifiez la sous-chaine, cela n'aura aucun impact sur la chaine d'origine.

## Voir aussi 

- [Documentation de TypeScript sur la fonction substring()](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [Article sur la manipulation de chaînes en TypeScript](https://blog.bitsrc.io/manipulating-strings-in-typescript-181e4bfe8d37)