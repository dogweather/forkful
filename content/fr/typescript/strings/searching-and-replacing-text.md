---
date: 2024-01-20 17:58:54.933514-07:00
description: "How to: En TypeScript, on utilise souvent `.replace()` pour manipuler\
  \ les cha\xEEnes de caract\xE8res. Voyons un exemple ."
lastmod: '2024-03-13T22:44:57.420755-06:00'
model: gpt-4-1106-preview
summary: "En TypeScript, on utilise souvent `.replace()` pour manipuler les cha\xEE\
  nes de caract\xE8res."
title: Recherche et remplacement de texte
weight: 10
---

## How to:
En TypeScript, on utilise souvent `.replace()` pour manipuler les chaînes de caractères. Voyons un exemple :

```typescript
const phrase: string = "Bonjour, TypeScript!";
const nouvellePhrase: string = phrase.replace("TypeScript", "le monde");

console.log(nouvellePhrase); // Affiche: Bonjour, le monde!
```

Et pour plusieurs changements avec l'utilisation des expressions régulières (RegEx):

```typescript
const texte: string = "Les pommes sont rouges. Les pommes sont juteuses.";
const regex: RegExp = /pommes/g;
const nouveauTexte: string = texte.replace(regex, "bananes");

console.log(nouveauTexte); // Affiche: Les bananes sont rouges. Les bananes sont juteuses.
```

## Deep Dive
Historiquement, le besoin de chercher et remplacer du texte est venu avec l'édition de documents. C'est un héritage des traitements de texte adapté au monde de la programmation. En TypeScript, `.replace()` est la méthode de prédilection mais il y a des alternatives comme les bibliothèques spécialisées qui offrent plus de fonctionnalités, comme `lodash` et son utilitaire `.replace()`.

La méthode `.replace()` permet les remplacements simples et prend en charge les expressions régulières pour plus de complexité. Attention, sans le flag `/g`, le remplacement se fait seulement pour la première occurrence.

## See Also
- MDN Web Docs, `.replace()`: [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- RegExp Guide: [https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions)
- Lodash Library: [https://lodash.com/docs/#replace](https://lodash.com/docs/#replace)
