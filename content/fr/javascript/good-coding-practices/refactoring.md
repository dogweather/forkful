---
date: 2024-01-26 01:40:31.201580-07:00
description: "Le Refactoring est le processus de restructuration du code informatique\
  \ existant sans en changer le comportement externe. Les programmeurs le font pour\u2026"
lastmod: '2024-03-13T22:44:58.284623-06:00'
model: gpt-4-0125-preview
summary: "Le Refactoring est le processus de restructuration du code informatique\
  \ existant sans en changer le comportement externe. Les programmeurs le font pour\u2026"
title: "R\xE9usinage"
weight: 19
---

## Quoi & Pourquoi ?
Le Refactoring est le processus de restructuration du code informatique existant sans en changer le comportement externe. Les programmeurs le font pour améliorer les attributs non fonctionnels du logiciel, rendant le code plus propre et plus efficace, ce qui simplifie la maintenance et facilite l'ajout de futures fonctionnalités.

## Comment faire :

Prenons un exemple simple où le refactoring peut rendre votre code plus concis et lisible. Ici, nous refactorisons une fonction qui calcule la somme d'un tableau de nombres.

Avant :
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Sortie : 10
```

Après :
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Sortie : 10
```

Voyez comment la méthode `reduce` diminue la taille de la fonction tout en conservant sa fonctionnalité intacte ? C’est ça, le refactoring.

## Plongée Profonde

Le refactoring n'est apparu comme une pratique formelle qu'avec la publication du livre de Martin Fowler "Refactoring : Améliorer la conception de code existant" en 1999. Ce livre, avec l'émergence du développement logiciel agile, a aidé à intégrer le refactoring dans le courant principal.

Décrire le refactoring comme un aspect du développement logiciel, c'est comme expliquer pourquoi vous rangeriez un atelier : vous le faites pour que la prochaine fois que vous devrez réparer quelque chose (dans ce cas, du code), vous passerez moins de temps à gérer le désordre et plus à résoudre le problème réel.

Quand nous parlons d'alternatives au refactoring, nous entrons dans une discussion plus large sur les stratégies de maintenance logicielle. On pourrait opter pour une réécriture complète, par exemple, mais cela est souvent plus coûteux et risqué. Refactorisez progressivement, et vous récolterez des bénéfices continus sans faire naufrage à cause d'une révision soudaine.

Le refactoring a été facilité par le développement d'environnements de développement intégrés (IDEs) et d'outils comme JSHint, ESLint et Prettier dans l'écosystème JavaScript, qui automatisent les vérifications de qualité du code et mettent en évidence les opportunités de refactoring.

Il s'agit de rendre le code propre, expressif et maintenable. Des algorithmes sophistiqués, des optimisations de structures de données ou même des changements d'architecture, comme passer du style de programmation procédural au fonctionnel, peuvent faire partie d'un processus de refactoring.

Le refactoring doit être effectué avec soin ; il est essentiel d'avoir un ensemble robuste de tests pour s'assurer que vos changements n'ont pas modifié de manière inattendue le comportement du logiciel, une autre raison pour laquelle le Développement Piloté par les Tests (TDD) s'harmonise bien avec le refactoring, puisqu'il offre ce filet de sécurité par défaut.

## Voir également

- Le livre sur le Refactoring de Martin Fowler : [Refactoring - Améliorer la conception de code existant](https://martinfowler.com/books/refactoring.html)
- Frameworks de Test JavaScript (pour garantir que le refactoring ne casse pas la fonctionnalité) :
  - Jest : [Jest - Test JavaScript Agréable](https://jestjs.io/)
  - Mocha : [Mocha - le framework de test JavaScript fun, simple, flexible](https://mochajs.org/)

- Outils pour la Qualité du Code et le Support au Refactoring :
  - ESLint : [ESLint - Linter JavaScript modulable](https://eslint.org/)
  - Prettier : [Prettier - Formateur de Code Opinioné](https://prettier.io/)
