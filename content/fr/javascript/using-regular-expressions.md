---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières sont des séquences de caractères formant un schéma de recherche. On les utilise pour fouiller, manipuler ou valider des chaînes de texte.

## Comment faire :
```Javascript
// Trouver tous les numéros dans une chaîne de texte
let texte = "Il y a 12 pommes et 33 bananes";
let regex = /\d+/g;
console.log(texte.match(regex)); // Affiche ["12", "33"]

// Valider le format d'un email
let email = "exemple@domaine.fr";
let regexEmail = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
console.log(regexEmail.test(email)); // Affiche true
```

## Exploration
Les expressions régulières existent depuis les années 1950; leur conception est attribuée à l'informaticien Stephen Cole Kleene. Il y a des alternatives comme le parcours de chaîne de caractères manuel, mais les regex sont plus concises et puissantes. En JavaScript, elles sont implémentées via l'objet `RegExp` et peuvent être utilisées avec différentes méthodes de chaîne, telles que `.match()`, `.replace()`, `.search()`, et `.split()`.

## Voir Aussi
- [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) - Guide complet sur les expressions régulières en JavaScript.
- [Regex101](https://regex101.com/) - Outil en ligne pour tester et apprendre les expressions régulières.
- [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp) - Référence W3Schools pour l'objet RegExp.
