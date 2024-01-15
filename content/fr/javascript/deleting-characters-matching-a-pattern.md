---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Javascript: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile lors de la manipulation de chaînes de caractères pour enlever des portions spécifiques ou pour nettoyer des données.

## Comment Faire

Il existe plusieurs façons de supprimer des caractères correspondant à un motif en utilisant Javascript. Voici deux méthodes couramment utilisées :

```Javascript
// Méthode 1 : Utiliser la méthode .replace() avec une expression régulière
let string = "Bonjour, mon nom est Jean.";
let modifiedString = string.replace(/o/g, ""); // Supprime toutes les lettres o de la chaîne
console.log(modifiedString); // Bnjur, mn nm est Jean.

// Méthode 2 : Utiliser la méthode .split() et .join()
let string = "Hello, my name is John.";
let modifiedString = string.split("l").join(""); // Split la chaîne à chaque "l" et rejoint les parties sans inclure les "l"
console.log(modifiedString); //Heo, my name is John.
```

Dans les deux cas, une nouvelle chaîne est retournée avec les caractères correspondant au motif supprimés.

## Plongeon Dans les Profondeurs

Lorsque vous utilisez la méthode .replace() avec une expression régulière, vous pouvez choisir d'utiliser le drapeau "g" pour indiquer si vous souhaitez trouver toutes les occurrences du motif ou seulement la première. La méthode .split() et .join() ne dispose pas de cette option, ce qui peut être un inconvénient dans certains cas.

De plus, il est important de noter que ces méthodes ne modifient pas la chaîne originale, elles retournent plutôt une nouvelle chaîne avec les modifications. Si vous souhaitez modifier la chaîne initiale, vous pouvez utiliser la variable qui stocke la nouvelle chaîne comme référence.

## Voir Aussi

- [Documentation MDN sur la méthode .replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Documentation MDN sur la méthode .split() et .join()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/split)