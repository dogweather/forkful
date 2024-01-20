---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Extraire des sous-chaînes consiste à obtenir une partie spécifique d'une chaîne de caractères.
C'est essentiel lorsque les programmeurs ont besoin de manipuler ou analyser certaines parties d'un texte ou des données.

## Comment faire:

Dans TypeScript, l'extraction de sous-chaînes peut être réalisée avec les méthodes `slice`, `substr` et `substring`. Jetons un coup d'œil à ces exemples:

```TypeScript
let string = "Bonjour tout le monde!";

// Utilisation de slice
console.log(string.slice(8, 12));  // sortie: "tout"

// Utilisation de substr
console.log(string.substr(8, 4));  // sortie: "tout"

// Utilisation de substring
console.log(string.substring(8, 12));  // sortie: "tout"
```

Chaque méthode retourne la sous-chaîne de "Bonjour tout le monde!" depuis l'index 8 jusqu'à l'index 12.

## Plongée profonde

Historiquement, `substr` et `substring` étaient des méthodes populaires pour l'extraction de sous-chaînes, mais leurs usages se sont mélangés avec le temps. Aujourd'hui, la plupart des développeurs préfèrent `slice` pour sa flexibilité et sa cohérence avec d'autres méthodes de tableau JavaScript.

En regardant les alternatives, il existe également des expressions régulières et l'utilisation de `split` pour extraire des sous-chaînes, bien que ces méthodes soient généralement plus adaptées aux cas plus complexes.

Quant aux détails de l'implémentation, `slice` et `substring` diffèrent légèrement dans leur comportement lorsqu'ils sont fournis avec des indices négatifs. `slice` considère un indice négatif comme une position à partir de la fin de la chaîne, tandis que `substring` traite cela comme un zéro.

## Voir Aussi

Pour plus d'informations sur l'extraction de sous-chaînes dans TypeScript, consultez ces liens intéressants:
- La documentation officielle de Mozilla sur les strings et leurs méthodes: [MDN String](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String)