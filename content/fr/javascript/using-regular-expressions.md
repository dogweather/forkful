---
title:                "L'utilisation des expressions régulières."
html_title:           "Javascript: L'utilisation des expressions régulières."
simple_title:         "L'utilisation des expressions régulières."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs utilisent-ils des expressions régulières?
Les expressions régulières, également connues sous le nom de "RegEx", sont des motifs utilisés pour rechercher et manipuler du texte dans les programmes informatiques. Elles offrent aux programmeurs un moyen puissant et flexible de trouver et de modifier des portions de texte dans leurs codes.

# Comment faire:
Voici quelques exemples de code en Javascript montrant comment utiliser des expressions régulières :

```Javascript
// Rechercher un nombre dans une chaîne de caractères
let string = "Il y a 5 pommes dans le panier.";
let pattern = /\d/; // utiliser l'expression régulière \d pour trouver un chiffre
let result = string.match(pattern);
console.log(result); // affiche 5

// Remplacer un mot dans une phrase
let sentence = "Je mange du pain au petit-déjeuner.";
let newWord = "croissant";
let oldWord = "pain";
let pattern = new RegExp(oldWord, 'g'); // utiliser la méthode RegExp pour créer une expression régulière avec le mot à remplacer et l'indicateur "g" pour le trouver partout dans la phrase
let newSentence = sentence.replace(pattern, newWord);
console.log(newSentence); // affiche "Je mange du croissant au petit-déjeuner."
```

# Plongée en profondeur:
Les expressions régulières sont nées dans les années 1950 avec le langage SNOBOL. Elles sont devenues populaires en informatique grâce aux langages Perl et Unix. Les autres alternatives pour la recherche de texte incluent les fonctions de chaîne de caractères en Javascript, mais les expressions régulières offrent une plus grande flexibilité et plus de fonctionnalités.

Outre la méthode `match()` utilisée dans l'exemple ci-dessus, Javascript propose également les méthodes `test()`, `replace()` et `split()` pour travailler avec des expressions régulières. Il est également possible d'utiliser des drapeaux tels que "i" pour ignorer la casse ou "g" pour trouver toutes les occurrences.

# Voir aussi:
Pour en savoir plus sur les expressions régulières en Javascript, consultez ces sources :
- [La documentation officielle de Mozilla](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_r%C3%A9guli%C3%A8res)
- [Regex101.com - un site pour tester et debugger des expressions régulières](https://regex101.com/)