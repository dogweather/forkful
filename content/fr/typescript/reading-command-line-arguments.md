---
title:                "Analyse des arguments en ligne de commande"
html_title:           "TypeScript: Analyse des arguments en ligne de commande"
simple_title:         "Analyse des arguments en ligne de commande"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur de logiciels, vous avez probablement entendu parler des arguments de ligne de commande ou des "command line arguments". Ces arguments sont des valeurs saisies par l'utilisateur lors de l'exécution d'un programme. Ils peuvent être très utiles car ils permettent à l'utilisateur de modifier rapidement et facilement le comportement du programme sans avoir à le modifier directement. Dans cet article, nous allons vous montrer comment lire et utiliser les arguments de ligne de commande en TypeScript.

## Comment faire

Pour lire les arguments de ligne de commande en TypeScript, il suffit d'accéder à l'objet global `process` et à sa propriété `argv`. Cette propriété contient un tableau des arguments saisis par l'utilisateur lors de l'exécution du programme.

```TypeScript
const args = process.argv;

// Si vous exécutez le programme avec la commande "node index.ts hello world",
// les arguments seront ["hello", "world"]
```

Vous pouvez également utiliser la fonction `slice()` pour obtenir une partie de ce tableau, en excluant le premier argument qui est toujours le chemin d'accès du programme.

```TypeScript
const args = process.argv.slice(2);

// Avec la même commande "node index.ts hello world",
// les arguments seront ["hello", "world"]
```

Maintenant que nous avons les arguments stockés dans une variable, nous pouvons les utiliser selon nos besoins. Par exemple, si nous voulons afficher un message de bienvenue en utilisant le premier argument, nous pouvons le faire en utilisant la notation de chaîne de modèle (string interpolation).

```TypeScript
const args = process.argv;

// Avec la commande "node index.ts John",
// le message de bienvenue sera "Welcome John!"
console.log(`Welcome ${args[2]}!`);
```

## Deep Dive

En plus de la fonction `slice()`, il existe également la fonction `substring()` pour obtenir une sous-chaîne à partir d'une chaîne. Nous pouvons utiliser cette fonction pour extraire des parties spécifiques d'un argument. Par exemple, si nous voulons obtenir les deux premiers caractères du premier argument, nous pouvons le faire comme ceci:

```TypeScript
const args = process.argv;

// Avec la commande "node index.ts hello",
// le résultat sera "he"
console.log(args[2].substring(0, 2));
```

Il convient également de noter que les arguments de ligne de commande sont toujours des chaînes de caractères, même s'ils ressemblent à des nombres ou à des booléens. Cela signifie que si nous voulons les utiliser comme des nombres, nous devons les convertir en utilisant les fonctions `parseInt()` ou `parseFloat()`.

## Voir aussi

- [Parcourir les arguments de ligne de commande en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-arguments-and-parameters-in-typescript)
- [Documentation sur l'objet process en Node.js](https://nodejs.org/api/process.html)