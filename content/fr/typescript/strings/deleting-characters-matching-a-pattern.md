---
date: 2024-01-20 17:43:01.260168-07:00
description: 'How to: (Comment faire :) Sortie .'
lastmod: '2024-04-05T21:53:58.984995-06:00'
model: gpt-4-1106-preview
summary: (Comment faire :) Sortie .
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## How to: (Comment faire :)
```TypeScript
let rawInput: string = "C3eci est un t3exte av3ec des ch1ffres!";
let cleanInput: string = rawInput.replace(/\d+/g, '');
console.log(cleanInput); // "Ceci est un texte avec des chiffres!"
```
Sortie :
```
Ceci est un texte avec des chiffres!
```

## Deep Dive (Plongée en profondeur)
Supprimer des caractères remonte aux premiers jours de la programmation quand la mémoire était précieuse et la clarté cruciale. À l'époque de JavaScript, la fonction `replace()` fut introduite, et elle a été héritée par TypeScript, un sur-ensemble typé de JavaScript. Alors que `replace()` remplace la première occurrence, `replace()` avec une expression régulière globale (`/g`) cible toutes les correspondances. Autrement, on pourrait utiliser une loop ou des fonctions de bibliothèques externes, mais dans TypeScript, les expressions régulières sont souvent la route la plus directe et la plus lisible.

## See Also (Voir aussi)
- MDN Documentation on `String.prototype.replace()`: [MDN replace](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- TypeScript Handbook: [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- Regular Expressions Guide: [Regex Guide](https://www.regular-expressions.info/)

N'oubliez pas que la maîtrise des expressions régulières ouvre un univers de possibilités pour manipuler des chaînes de caractères, bien au-delà de la simple suppression de caractères!
