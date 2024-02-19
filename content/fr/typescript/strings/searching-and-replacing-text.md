---
aliases:
- /fr/typescript/searching-and-replacing-text/
date: 2024-01-20 17:58:54.933514-07:00
description: "Rechercher et remplacer du texte, c'est comme jouer \xE0 cache-cache\
  \ avec des mots pour les transformer ou les corriger. Les programmeurs le font pour\
  \ mettre\u2026"
lastmod: 2024-02-18 23:09:08.465289
model: gpt-4-1106-preview
summary: "Rechercher et remplacer du texte, c'est comme jouer \xE0 cache-cache avec\
  \ des mots pour les transformer ou les corriger. Les programmeurs le font pour mettre\u2026"
title: Recherche et remplacement de texte
---

{{< edit_this_page >}}

## What & Why?
Rechercher et remplacer du texte, c'est comme jouer à cache-cache avec des mots pour les transformer ou les corriger. Les programmeurs le font pour mettre à jour des données, corriger des erreurs, ou modifier le code plus efficacement.

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
