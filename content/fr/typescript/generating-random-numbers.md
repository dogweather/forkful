---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:56.996809-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Générer des nombres aléatoires, c'est tirer des chiffres au sort, comme une loterie numérique. En programmation, on les utilise pour tout, des jeux à la sécurité informatique.

## How to: (Comment faire :)
Voici du code TypeScript qui lance les dés pour vous:

```typescript
function obtenirNombreAleatoire(max: number): number {
  return Math.floor(Math.random() * max);
}

console.log(obtenirNombreAleatoire(10)); // Peut sortir n'importe quel nombre entre 0 et 9
```

Et si on veut un nombre entre deux valeurs, min et max ?

```typescript
function obtenirNombreAleatoireEntre(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(obtenirNombreAleatoireEntre(1, 10)); // Un nombre entre 1 et 10, inclus
```

## Deep Dive (Plongée en profondeur)
Historiquement, générer de l'aléatoire en informatique, c'est un paradoxe : un ordinateur suit des instructions précises, comment peut-il être imprévisible ? En réalité, `Math.random()` en JavaScript (dont TypeScript est un sur-ensemble) n'est pas véritablement aléatoire; c'est pseudo-aléatoire, basé sur une formule déterministe mais complexe. 

Alternatives ? Il y a des bibliothèques spécialisées, comme `crypto`, qui proposent des nombres plus imprévisibles et mieux adaptés à la cryptographie. Pourquoi ne pas les utiliser tout le temps ? Ils sont plus lents et souvent overkill pour des jeux ou des simulations.

En parlant d'implémentation, `Math.random()` de TypeScript renvoie un nombre à virgule flottante entre 0 (inclus) et 1 (exclus). Le truc avec `Math.floor(Math.random() * max)` est une astuce classique pour arrondir à l'entier inférieur et étendre la plage de valeurs.

## See Also (Voir aussi)
- MDN Web Docs sur `Math.random()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Node.js `crypto` module: https://nodejs.org/api/crypto.html
- Une discussion sur Stack Overflow concernant la génération de nombres aléatoires en TypeScript: https://stackoverflow.com/questions/tagged/typescript+random

C'est tout pour l'instant. À la prochaine pour d'autres snippets de code et astuces TypeScript!
