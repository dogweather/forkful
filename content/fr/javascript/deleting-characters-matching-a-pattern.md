---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Javascript: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Supprimer des caractères correspondant à un schéma est une opération courante en programmation où l'on souhaite éliminer certaines parties de données selon un motifs spécifiques. Les programmeurs le font souvent lorsqu'ils doivent nettoyer ou manipuler des données pour les rendre plus lisibles ou utiles.

## Comment faire:

Voici deux méthodes pour supprimer des caractères correspondant à un schéma en Javascript:

```javascript
// Méthode 1: Utiliser la fonction replace() avec une expression régulière
let str = "abc123";
str = str.replace(/[0-9]/g, ""); // Supprime tous les chiffres
console.log(str); // Output: abc

// Méthode 2: Utiliser la méthode split() et join()
let str = "abc123";
str = str.split(/[0-9]/).join(""); // Splitte la chaîne selon les chiffres et join() les morceaux
console.log(str); // Output: abc
```

## Plongée en profondeur:

Les expressions régulières utilisées dans les exemples ci-dessus sont des patrons spécifiques qui correspondent à certains types de caractères. Les développeurs peuvent également utiliser des fonctions comme substring() ou slice() pour supprimer des caractères selon leur position dans une chaîne. Alternativement, certains langages de programmation proposent des méthodes dédiées pour supprimer des caractères correspondant à un modèle.

Les expressions régulières sont apparues pour la première fois dans les années 1950 et sont maintenant largement utilisées dans de nombreux langages de programmation, y compris Javascript, pour la manipulation de chaînes. Bien que cela puisse sembler intimidant au début, une compréhension de base des expressions régulières peut grandement faciliter le traitement de données.

## Voir aussi:

Pour en savoir plus sur les expressions régulières et leur utilisation en Javascript, vous pouvez consulter ces ressources:

- [Mozilla Developer Network - Expressions Régulières en Javascript] (https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_régulières)
- [w3schools - Javascript Expressions Régulières] (https://www.w3schools.com/js/js_regexp.asp)
- [Codeacademy - Expressions Régulières en Javascript] (https://www.codecademy.com/fr/courses/introduction-to-javascript/lessons/advanced-regular-expressions-in-js/exercises/what-youll-build)