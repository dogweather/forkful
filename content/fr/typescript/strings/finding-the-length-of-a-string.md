---
title:                "Trouver la longueur d'une chaîne de caractères"
aliases:
- /fr/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:32.644278-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Trouver la longueur d'une chaîne de caractères, c'est découvrir combien de caractères elle contient. Les programmeurs le font pour valider des saisies utilisateur, tronquer du texte ou tout simplement pour manipuler des données textuelles avec précision.

## Comment faire :
Voilà un exemple simple en TypeScript. On utilise la propriété `length` d'une chaîne.

```TypeScript
let message: string = "Bonjour!";
console.log(message.length);  // Affiche: 8
```

Notez que le caractère `!` est inclus dans le décompte.

Pour une chaîne vide, la longueur est 0.

```TypeScript
let vide: string = "";
console.log(vide.length);  // Affiche: 0
```

## Exploration approfondie
Historiquement, connaître la longueur d'une chaîne a toujours été fondamental dans la programmation car elle permet de juger de la quantité de données traitées. En JavaScript, et donc en TypeScript, la longueur est récupérée directement depuis la propriété `length` de l'objet chaîne. C'est une opération de complexité O(1), ce qui signifie qu'elle ne dépend pas de la longueur de la chaîne.

Il existe aussi d'autres façons de manipuler et mesurer les chaînes. Par exemple, avec l'itération sur les chaînes utilisant des boucles ou des fonctions de haut niveau. Mais ces méthodes sont moins directes et moins performantes pour obtenir simplement la longueur.

TypeScript, étant un sur-ensemble de JavaScript, partage les mêmes méthodes de chaînes. Cependant, TypeScript ajoute une vérification de type lors de la compilation pour éviter les erreurs de manipulation de chaînes non définies ou d'autres types non compatibles.

## À voir également
- Documentation TypeScript sur les types de base : [TypeScript Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- Mozilla Developer Network sur les chaînes de caractères en JavaScript : [MDN Strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Chapitre sur les chaînes de caractères et leurs propriétés dans "Eloquent JavaScript" : [Eloquent JavaScript: Strings and their properties](https://eloquentjavascript.net/01_values.html#p_4y2Z4w5QyX)
