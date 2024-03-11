---
date: 2024-01-20 17:46:50.792503-07:00
description: "L'extraction de sous-cha\xEEnes consiste \xE0 prendre des morceaux sp\xE9\
  cifiques d'une cha\xEEne de caract\xE8res. Les d\xE9veloppeurs le font pour analyser,\
  \ manipuler ou\u2026"
lastmod: '2024-03-11T00:14:31.440811-06:00'
model: gpt-4-1106-preview
summary: "L'extraction de sous-cha\xEEnes consiste \xE0 prendre des morceaux sp\xE9\
  cifiques d'une cha\xEEne de caract\xE8res. Les d\xE9veloppeurs le font pour analyser,\
  \ manipuler ou\u2026"
title: "Extraction de sous-cha\xEEnes"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
L'extraction de sous-chaînes consiste à prendre des morceaux spécifiques d'une chaîne de caractères. Les développeurs le font pour analyser, manipuler ou afficher des données textuelles de manière plus pertinente.

## How to: (Comment faire :)
Voici comment extraire des sous-chaînes en TypeScript avec `substring`, `slice` et `substr` (déprécié mais bon à connaître).

```typescript
let text: string = "TypeScript est génial!";

// Utiliser substring
let subText1: string = text.substring(0, 10); // "TypeScript"

// Utiliser slice
let subText2: string = text.slice(11, 18); // "est"

// Utiliser substr (déprécié)
let subText3: string = text.substr(19, 7); // "génial!"

console.log(subText1); // Affiche: TypeScript
console.log(subText2); // Affiche: est
console.log(subText3); // Affiche: génial!
```

## Deep Dive (Plongée en profondeur)
Historiquement, `substr` fut longtemps utilisé malgré une implémentation et un usage inconsistants entre les navigateurs. TypeScript, comme super-ensemble de JavaScript, en hérite mais privilégie `substring` et `slice` pour leur comportement plus prédictible.

Alternatives ? On pourrait aussi utiliser des expressions régulières avec `match` ou des méthodes orientées sur les tableaux comme `split` et `join`.

Pour ce qui est de l'implémentation, `substring` et `slice` prennent deux indices : début et fin alors que `substr` prend l'indice de départ et le nombre de caractères. Petit détail : `slice` peut prendre des indices négatifs pour travailler en arrière à partir de la fin.

## See Also (Voir aussi)
- Documentation TypeScript sur les chaînes de caractères : [TypeScript String](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#strings)
- MDN Web Docs sur `slice()`, `substring()`, et `substr()`: [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice), [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring), [String.prototype.substr() (déprécié)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- Regex one-liners pour l'extraction de sous-chaînes : [Regexr](https://regexr.com/)
- Un guide pour manipuler les chaînes de caractères en JavaScript (applicable à TypeScript) : [String Manipulation in JavaScript](https://exploringjs.com/impatient-js/ch_strings.html)
