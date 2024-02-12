---
title:                "Trouver la longueur d'une chaîne de caractères"
aliases:
- fr/javascript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:48.924493-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

En JavaScript, connaître la longueur d'une chaîne signifie compter le nombre de caractères qu'elle contient. C'est essentiel pour valider des entrées, manipuler du texte ou tout simplement savoir quel est notre "budget" de caractères dans différentes situations.

## Comment faire :

```javascript
let message = "Bonjour le monde!";
let longueur = message.length; // On obtient la longueur de la chaîne

console.log(longueur); // Affiche 17
```

```javascript
let vide = "";
console.log(vide.length); // Affiche 0, même les chaînes vides ont une longueur
```

```javascript
let avecEspaces = "   espaces   ";
console.log(avecEspaces.length); // Affiche 13, les espaces comptent!
```

## Exploration en profondeur

Historiquement, la propriété `.length` existe depuis les premiers jours de JavaScript ; elle est simple et directe. Il n'y a pas vraiment d'alternatives pour obtenir la longueur d'une chaîne en JS - `.length` est votre outil de base. Côté implémentation, `.length` est une propriété prédéfinie du prototype String, ce qui la rend disponible pour toutes les chaînes.

Une chose à garder à l'esprit est que `.length` renvoie le nombre d'unités de code UTF-16 dans la chaîne, pas nécessairement le nombre de caractères Unicode "réels", ce qui pourrait être différent à cause des caractères composés (ceux utilisant des "surrogates pairs").

```javascript
let emoji = "😀";
console.log(emoji.length); // Affiche 2, car les emojis peuvent être composés de plusieurs unités de code
```

Pour compter des caractères dits "complexes", vous pourriez devoir utiliser des fonctionnalités de l'ES2015 comme `[...str].length` ou `Array.from(str).length`, qui traitent correctement les caractères à plusieurs unités de code.

## Voir également

- MDN Web Docs sur `.length`: [String.length - JavaScript | MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- ECMAScript 2015 (ES6) et l'utilisation des points de code Unicode : [ECMAScript 2015 (6th Edition, ECMA-262)](https://www.ecma-international.org/ecma-262/6.0/index.html)
- Un aperçu approfondi des unités de code UTF-16 : [Understanding UTF-16](https://unicodebook.readthedocs.io/unicode_encodings.html#utf-16le)
