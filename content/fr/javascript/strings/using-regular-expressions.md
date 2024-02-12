---
title:                "Utilisation des expressions régulières"
aliases:
- /fr/javascript/using-regular-expressions.md
date:                  2024-02-03T19:17:50.128733-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des expressions régulières"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Les expressions régulières (regex) en JavaScript sont des motifs utilisés pour correspondre à des combinaisons de caractères dans des chaînes de caractères. Les programmeurs les utilisent pour rechercher, extraire et manipuler du texte, permettant ainsi de puissantes opérations de traitement de chaînes avec du code concis.

## Comment faire :

### Correspondance de base

Pour commencer, vous pouvez créer un motif regex simple et l'utiliser pour trouver des correspondances dans une chaîne de caractères. Ici, nous trouverons le mot "code" :

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Utiliser `String.prototype.match()`

Pour récupérer un tableau de correspondances :

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Recherche globale

Pour trouver toutes les correspondances, utilisez le drapeau `g` :

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Correspondance sans tenir compte de la casse

Le drapeau `i` ignore la casse :

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Remplacer du texte

Utilisez `String.prototype.replace()` pour remplacer des parties de la chaîne :

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### Utilisation de groupes

Les groupes peuvent capturer des parties du motif :

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### Bibliothèques tierces

Bien que les capacités regex intégrées de JavaScript soient puissantes, certaines tâches pourraient être simplifiées avec des bibliothèques comme `XRegExp`. Elle offre une syntaxe et des drapeaux supplémentaires, rendant les motifs complexes plus lisibles :

```javascript
// Exemple de la bibliothèque XRegExp
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

Cet extrait démontre l'utilisation de `XRegExp` pour correspondre à tous les mots Unicode dans une chaîne de caractères, montrant la capacité de la bibliothèque à gérer des ensembles de caractères étendus au-delà des capacités intégrées de JavaScript.
