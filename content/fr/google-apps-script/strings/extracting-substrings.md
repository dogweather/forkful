---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:48.339093-07:00
description: "Extraire des sous-cha\xEEnes consiste \xE0 prendre une portion d'une\
  \ cha\xEEne de caract\xE8res - en cr\xE9ant essentiellement une nouvelle cha\xEE\
  ne \xE0 partir d'une partie\u2026"
lastmod: '2024-03-13T22:44:57.171831-06:00'
model: gpt-4-0125-preview
summary: "Extraire des sous-cha\xEEnes consiste \xE0 prendre une portion d'une cha\xEE\
  ne de caract\xE8res - en cr\xE9ant essentiellement une nouvelle cha\xEEne \xE0 partir\
  \ d'une partie\u2026"
title: "Extraction de sous-cha\xEEnes"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Extraire des sous-chaînes consiste à prendre une portion d'une chaîne de caractères - en créant essentiellement une nouvelle chaîne à partir d'une partie d'une chaîne existante. Les programmeurs font cela pour une myriade de raisons, y compris l'analyse de données, la manipulation de texte pour les interfaces utilisateur, ou le traitement des entrées pour diverses applications, faisant de l'extraction de sous-chaînes un outil polyvalent dans tout arsenal de script.

## Comment faire :

Dans Google Apps Script, qui est basé sur JavaScript moderne, l'extraction de sous-chaînes peut être réalisée par plusieurs méthodes, y compris `substring()`, `substr()`, et `slice()`. Chacune a ses nuances, mais elles servent toutes à extraire des caractères spécifiés d'une chaîne.

```javascript
// Exemple utilisant substring()
var str = "Bonjour, le monde!";
var result = str.substring(0, 7);
console.log(result); // Sortie : Bonjour

// Exemple utilisant substr()
var resultSubstr = str.substr(9, 5);
console.log(resultSubstr); // Sortie : monde

// Exemple utilisant slice()
var resultSlice = str.slice(-7);
console.log(resultSlice); // Sortie : monde!
```

Chaque méthode prend deux arguments : la position de départ et, sauf pour `slice()` qui peut accepter des indices négatifs pour commencer à partir de la fin, la position de fin ou le nombre de caractères à extraire. Il est important de noter que la chaîne originale reste inchangée après ces opérations, car elles renvoient de nouvelles valeurs de chaîne.

## Approfondissement

Historiquement, les méthodes JavaScript pour extraire des sous-chaînes ont été une source de confusion en raison de leurs noms et fonctionnalités similaires. Cependant, dans Google Apps Script et JavaScript moderne, `substring()` et `slice()` sont les plus fréquemment utilisés, `substr()` étant considéré comme obsolète. C'est important à noter pour ceux qui écrivent du code pérenne.

La principale différence entre `substring()` et `slice()` est la manière dont ils gèrent les indices négatifs ; `substring()` traite les indices négatifs comme 0, tandis que `slice()` peut accepter un indice négatif pour commencer l'extraction à partir de la fin de la chaîne. Cela rend `slice()` particulièrement pratique pour les cas où la longueur exacte de la chaîne pourrait ne pas être connue ou lorsqu'il est nécessaire d'extraire depuis la fin.

Lors de la décision sur la méthode à utiliser pour l'extraction de sous-chaînes, le choix se résume souvent aux exigences spécifiques de l'opération (par exemple, si le traitement des indices négatifs est bénéfique) et aux normes de codage personnelles ou d'équipe. Bien qu'il n'existe pas de meilleure pratique universelle, comprendre les différences subtiles et les implications sur les performances peut aider à prendre une décision éclairée.
