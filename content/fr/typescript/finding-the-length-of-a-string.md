---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trouver la longueur d'une chaîne en TypeScript

## Qu'est-ce et pourquoi?

Trouver la longueur d'une chaîne est une tâche consistant à compter le nombre de caractères dans une chaîne. C'est utile pour évaluer les saisies des utilisateurs, manipuler du texte, valider des formulaires et plus encore.

## Comment faire:

La propriété `.length` en TypeScript est ce qui nous permet d'obtenir la longueur d'une chaîne. Très simple, voyons comment ça fonctionne.

```TypeScript
let maChaine: string = "Bonjour, monde!";
console.log(maChaine.length); // Résultat: 15
```

Dans cet exemple, la longueur de "Bonjour, monde!" est de 15 caractères.

## Deep Dive: 

L'idée de trouver la longueur d'une chaîne a existé depuis les premiers jours de la programmation, elle est devenue une fonction fondamentale dans tous les langages modernes. En TypeScript, la propriété `.length` compte aussi les espaces et les caractères spéciaux.

Cependant, il existe d'autres méthodes pour trouver la longueur d'une chaîne, comme utiliser une boucle pour parcourir chaque caractère de la chaîne, mais `.length` est de loin la méthode la plus simple et la plus efficace.

En terme de détails d'implémentation, `.length` est une propriété incorporée du type `string` en TypeScript (et JavaScript). Il n'est pas nécessaire d'écrire une fonction distincte pour ça.

## Voir Aussi: 

1. [Documentation officielle TypeScript](https://www.typescriptlang.org/docs/)
2. [MDN Web Docs: Propriété String.prototype.length](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/length)
3. [StackOverflow: Questions sur String Length en TypeScript](https://stackoverflow.com/questions/tagged/typescript+string-length)