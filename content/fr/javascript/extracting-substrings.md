---
title:                "Javascript: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser l'extraction de sous-chaînes (substrings) en Javascript?

L'extraction de sous-chaînes est une technique couramment utilisée en programmation Javascript pour récupérer une partie spécifique d'une chaîne de caractères. Cela peut être utile dans de nombreux cas, tels que la manipulation de données saisies par l'utilisateur ou la recherche de mots clés dans un texte.

## Comment le faire en pratique

Pour extraire une sous-chaîne en Javascript, nous pouvons utiliser deux méthodes: `substring()` et `slice()`. Voyons un exemple pratique:

```
// Définir une chaîne de caractères
let str = "Je suis un programmeur Javascript";

// Utiliser la méthode substring()
let sub1 = str.substring(10, 20);
console.log(sub1); // "programmeur"

// Utiliser la méthode slice()
let sub2 = str.slice(3);
console.log(sub2); // "suis un programmeur Javascript"
```

Dans cet exemple, nous avons défini une chaîne de caractères et extrait une sous-chaîne en utilisant les méthodes `substring()` et `slice()`. La première méthode utilise deux paramètres pour indiquer les positions de début et de fin de la sous-chaîne à extraire, tandis que la seconde prend un seul paramètre pour indiquer la position de début et extrait le reste de la chaîne.

## Plongeons plus profondément

Il est important de noter que les index de chaîne en Javascript commencent à zéro. Cela signifie que le premier caractère d'une chaîne aura un index de 0, le deuxième aura un index de 1, et ainsi de suite. De plus, nous pouvons également utiliser des valeurs négatives pour indiquer des positions à partir de la fin de la chaîne. Par exemple, si nous voulons extraire les trois derniers caractères d'une chaîne, nous pouvons utiliser `slice(-3)`.

Un autre avantage de l'extraction de sous-chaînes est la possibilité de combiner différentes méthodes pour obtenir les résultats souhaités. Par exemple, nous pouvons utiliser à la fois `substring()` et `indexOf()` pour extraire une sous-chaîne à partir d'un mot clé spécifique dans une grande chaîne de caractères.

## Voir aussi
- [Documentation sur la méthode substring() en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/substring)
- [Documentation sur la méthode slice() en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)
- [Autres méthodes de manipulation de chaînes de caractères en Javascript](https://www.datacamp.com/community/tutorials/15-string-manipulation-in-javascript)