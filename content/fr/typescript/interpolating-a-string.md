---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:51:38.160284-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
L'interpolation de chaînes permet d'insérer des variables ou des expressions au sein d'une chaîne de caractères. Les programmeurs l'utilisent pour dynamiser du texte et simplifier la concaténation.

## Comment faire :
```typescript
let utilisateur = 'Marie';
let message = `Bonjour ${utilisateur}, comment ça va ?`;

console.log(message);  // "Bonjour Marie, comment ça va ?"
```
Dans cet exemple, la syntaxe avec les backticks (``) et le signe dollar suivi des accolades (`${}`) est utilisée pour interpoler la variable `utilisateur` dans le message.

## Exploration approfondie
Historiquement, l'interpolation de chaînes était plus laborieuse en JavaScript, nécessitant d'utiliser l'opérateur `+` pour concaténer des variables et des chaînes littérales :

```javascript
var utilisateur = 'Marie';
var message = 'Bonjour ' + utilisateur + ', comment ça va ?';
```

Avec ES6, TypeScript a adopté les template strings, simplifiant grandement l'interpolation. Il suffit d'utiliser des backticks et la syntaxe `${expression}` pour insérer des valeurs dans une chaîne de caractères.

Alternative, vous pourriez toujours utiliser la concaténation (+), mais c'est moins lisible et plus sujet aux erreurs.

Du point de vue de l'implémentation, l'interpolation de chaînes dans TypeScript est convertie en appel de fonction. Par exemple, `\`Bonjour ${utilisateur}\`` devient `Bonjour ".concat(utilisateur, " ")` après transpilation en ES5, garantissant que votre code fonctionne même sur d'anciens navigateurs.

## Voir aussi
- [Documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
- [MDN Web Docs sur Template Strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
