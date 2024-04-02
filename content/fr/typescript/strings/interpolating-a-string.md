---
date: 2024-01-20 17:51:38.160284-07:00
description: "L'interpolation de cha\xEEnes permet d'ins\xE9rer des variables ou des\
  \ expressions au sein d'une cha\xEEne de caract\xE8res. Les programmeurs l'utilisent\
  \ pour\u2026"
lastmod: '2024-03-13T22:44:57.421805-06:00'
model: gpt-4-1106-preview
summary: "L'interpolation de cha\xEEnes permet d'ins\xE9rer des variables ou des expressions\
  \ au sein d'une cha\xEEne de caract\xE8res. Les programmeurs l'utilisent pour\u2026"
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

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
