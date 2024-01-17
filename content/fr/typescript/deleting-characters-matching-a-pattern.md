---
title:                "Suppression de caractères correspondant à un modèle"
html_title:           "TypeScript: Suppression de caractères correspondant à un modèle"
simple_title:         "Suppression de caractères correspondant à un modèle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

"## Quoi & Pourquoi?"
Supprimer des caractères correspondant à un modèle est une technique courante utilisée par les programmeurs pour manipuler des chaînes de caractères. Cela permet de supprimer rapidement et facilement des parties précises d'une chaîne de caractères, ce qui peut être utile lors de la manipulation de données.

"## Comment faire:"
Voici quelques exemples de code TypeScript pour illustrer comment supprimer des caractères correspondant à un modèle:

```TypeScript 
const string = "Voici une chaîne de caractères.";

// Supprimer tous les caractères qui ne sont pas des lettres
let newString = string.replace(/[^a-zA-Z]/g, "");

console.log(newString); // Vounechaînedecaractères

// Supprimer les chiffres d'une chaîne de caractères
let newString = string.replace(/[0-9]/g, "");

console.log(newString); // Voiciunechaînedecaractères
```
Dans ces exemples, nous utilisons la méthode replace() de TypeScript pour remplacer les caractères correspondant au modèle (indiqué entre les deux barres obliques) par une chaîne de caractères vide.

"## Plongée en profondeur:"
La suppression de caractères correspondant à un modèle est une technique qui existe depuis longtemps et qui est utilisée dans de nombreux langages de programmation, notamment en JavaScript et en PHP. Elle est utile pour nettoyer et manipuler des données textuelles, mais il est important de garder à l'esprit qu'elle peut également modifier les données d'origine.

Il existe également d'autres moyens de supprimer des caractères correspondant à un modèle, tels que l'utilisation de la méthode match() de TypeScript pour récupérer les parties de la chaîne correspondant au modèle plutôt que de les supprimer.

"## Voir aussi:"
Pour en savoir plus sur la suppression de caractères correspondant à un modèle en TypeScript, vous pouvez consulter la documentation officielle de TypeScript sur les expressions régulières: https://www.typescriptlang.org/docs/handbook/regular-expressions.html
Vous pouvez également consulter des tutoriels et des exemples en ligne pour vous familiariser davantage avec cette technique.