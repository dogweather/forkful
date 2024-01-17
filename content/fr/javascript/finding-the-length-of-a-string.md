---
title:                "Trouver la longueur d'une chaîne de caractères."
html_title:           "Javascript: Trouver la longueur d'une chaîne de caractères."
simple_title:         "Trouver la longueur d'une chaîne de caractères."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Trouver la longueur d'une chaîne de caractères est une opération courante dans le développement en Javascript. Cela permet aux programmeurs de connaître la taille d'une chaîne de caractères et de travailler avec celle-ci de manière efficace dans leurs programmes.

## Comment faire:
Voici un exemple de code pour trouver la longueur d'une chaîne de caractères en Javascript:

```Javascript
let chaine = "Bonjour le monde";
let longueur = chaine.length;
console.log(longueur); // Output : 16 
```

## Plongée en profondeur:
Trouver la longueur d'une chaîne de caractères est une fonctionnalité de base dans plusieurs langages de programmation, comme C et Java. En Javascript, il existe également d'autres méthodes pour trouver la longueur d'une chaîne, telles que la méthode `string.length` et la méthode `string.size`.

Il est important de noter que la longueur d'une chaîne de caractères peut varier en fonction de la méthode utilisée. Par exemple, la méthode `string.length` comptera le nombre de caractères Unicode dans la chaîne, tandis que la méthode `string.size` comptera le nombre d'octets nécessaires pour stocker la chaîne.

## Voir aussi:
- [Documentation sur la méthode `length` de Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length)
- [Documentation sur la méthode `size` de Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/size)