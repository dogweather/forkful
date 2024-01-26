---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:48.246349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Extraire des sous-chaînes, c'est sélectionner des bouts spécifiques d'une chaîne de caractères. On le fait pour analyser, manipuler ou valider des données textuelles à la volée.

## Comment faire :
Voici trois méthodes courantes pour extraire des sous-chaînes en JavaScript :

```Javascript
let texte = "Bonjour, le monde!";

// Méthode slice(start, end)
let sousChaine1 = texte.slice(0, 7); // "Bonjour"
console.log(sousChaine1);

// Méthode substring(start, end)
let sousChaine2 = texte.substring(8, 11); // "le"
console.log(sousChaine2);

// Méthode substr(start, length) - Attention: obsolète!
let sousChaine3 = texte.substr(0, 7); // "Bonjour"
console.log(sousChaine3);
```

## Plongée profonde
`slice`, `substring`, `substr`... ça peut faire tourner la tête. Historiquement, `substr` était là en premier mais elle a été jugée obsolète, donc autant s'en tenir à `slice` et `substring`. Ils sont similaires avec une différence clé : `slice` peut prendre des valeurs négatives pour partir de la fin, tandis que `substring` interprète les valeurs négatives comme `0`. Le choix entre `slice` et `substring` se base sur les besoins spécifiques de manipulation des chaînes de votre projet.

## Voir aussi
- MDN Web Docs sur `slice()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- MDN Web Docs sur `substring()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- Discussion sur Stack Overflow sur la différence entre `slice()`, `substring()` et `substr()`: https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring
