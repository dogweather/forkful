---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "TypeScript: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi 

Vous pouvez avoir besoin de supprimer des caractères correspondant à un modèle pour nettoyer des données ou pour filtrer des résultats de recherche. Cela peut également être utile pour éliminer des symboles indésirables dans une chaîne de caractères.

## Comment faire 

```TypeScript
const string = "This is an example string with #hashtags and @mentions."
const result = string.replace(/[#@]/g, "")
console.log(result) // Output: This is an example string with hashtags and mentions.
``` 

Dans cet exemple, nous utilisons la méthode `replace()` pour remplacer tous les caractères correspondant au modèle `/[#@]/g` – qui contient ici les symboles # et @ - par une chaîne vide. Cela nous donne une nouvelle chaîne sans ces caractères.

## Plongée en profondeur 

Cette méthode de suppression de caractères basée sur un modèle utilise les expressions régulières, qui sont des motifs utilisés pour correspondre à des parties spécifiques d'une chaîne de caractères. Dans l'exemple ci-dessus, le modèle `/[#@]/g` contient deux caractères, mais on peut également spécifier une séquence de caractères plus complexe pour supprimer.

## Voir aussi 

- [Documentation de la méthode `replace()` en TypeScript] (https://www.typescriptlang.org/docs/handbook/functions.html#the-this-parameter)
- [Guide pour utiliser les expressions régulières en TypeScript] (https://www.typescriptlang.org/docs/handbook/regexp.html)
- [Un guide pour maîtriser les expressions régulières] (https://javascript.info/regular-expressions)