---
date: 2024-01-20 17:42:22.042815-07:00
description: "En JavaScript, supprimer des caract\xE8res qui correspondent \xE0 un\
  \ motif, c'est comme filtrer les grains de sable d'un d\xE9sert - on ne garde que\
  \ ce qu'on veut.\u2026"
lastmod: '2024-02-25T18:49:54.892410-07:00'
model: gpt-4-1106-preview
summary: "En JavaScript, supprimer des caract\xE8res qui correspondent \xE0 un motif,\
  \ c'est comme filtrer les grains de sable d'un d\xE9sert - on ne garde que ce qu'on\
  \ veut.\u2026"
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
---

{{< edit_this_page >}}

## What & Why?
En JavaScript, supprimer des caractères qui correspondent à un motif, c'est comme filtrer les grains de sable d'un désert - on ne garde que ce qu'on veut. On le fait pour nettoyer des données, valider des entrées ou transformer du texte.

## How to:
Utilisons `replace()` et les expressions régulières pour supprimer les chiffres d'une chaîne de caractères.

```Javascript
let phrase = "L'année est 2021.";
let nouvellePhrase = phrase.replace(/[0-9]/g, '');
console.log(nouvellePhrase); // "L'année est ."
```

Facile, non ? Maintenant, on enlève les ponctuations :

```Javascript
let sansPonctuation = nouvellePhrase.replace(/[.,\/#!$%\^&\*;:{}=\-_`~()]/g, '');
console.log(sansPonctuation); // "L'année est"
```

## Deep Dive
JavaScript a adopté les expressions régulières (regex) dès ses débuts, inspiré par des langages comme Perl. Pour enlever des caractères, le `replace()` est notre outil de prédilection. Mais attention, `replace()` sans le flag global `g` ne retirera que la première occurrence. Pensez aux alternatives comme les boucles ou des librairies comme Lodash si votre motif devient complexe à gérer avec regex.

Détails d'implémentation :
- `\d` est un raccourci pour `[0-9]`.
- Ajouter `i` à la fin de la regex pour ignorer la casse.
- Pour des performances optimales, compilez votre regex si vous l'utilisez plusieurs fois.

## See Also
- MDN Web Docs sur expressions régulières : [Guide MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions)
- JavaScript.info sur les méthodes de strings et regex : [JavaScript.info](https://javascript.info/regexp-introduction)
- Un aperçu détaillé sur `replace()` : [Documentation sur String.prototype.replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
