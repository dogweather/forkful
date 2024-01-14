---
title:    "Javascript: Capitaliser une chaîne de caractères"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Vous êtes probablement familier avec le concept de capitalisation de chaînes de caractères dans les langages de programmation. Mais pourquoi voudriez-vous le faire en premier lieu ? La réponse est simple : la capitalisation permet de rendre vos données plus cohérentes et plus lisibles dans vos scripts Javascript.

## Comment faire 

Il existe plusieurs façons d'implémenter la capitalisation de chaînes de caractères en Javascript. Voici deux exemples basiques avec leurs résultats :

```Javascript
var nom = "julien";
console.log(nom.toUpperCase()); // affichera "JULIEN"
```

```Javascript
var titre = "javascript est fantastique";
console.log(titre.charAt(0).toUpperCase() + titre.slice(1)); // affichera "Javascript est fantastique"
```

Comme vous pouvez le voir, la première méthode utilise la méthode `toUpperCase()` pour convertir une chaîne de caractères entière en majuscules. La seconde méthode utilise une combinaison de méthodes pour convertir seulement la première lettre en majuscule tout en conservant le reste de la chaîne en minuscules.

## Plongée en profondeur 

Maintenant que vous savez comment capitaliser une chaîne de caractères en Javascript, plongeons un peu plus en profondeur pour comprendre ce qui se passe réellement. En Javascript, les chaînes de caractères sont des objets et ont donc accès à des méthodes, comme `toUpperCase()` et `charAt()`. En utilisant ces méthodes et en combinant avec d'autres outils comme `slice()`, vous pouvez manipuler vos chaînes de caractères selon vos besoins.

## Voir aussi 

Pour en savoir plus sur la manipulation de chaînes de caractères en Javascript, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Javascript sur les méthodes de chaînes de caractères] (https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String).
- [Les différentes façons de capitaliser une chaîne de caractères en Javascript] (https://dev.to/samanthaming/capitalize-the-first-letter-of-a-string-in-javascript-2bkf).
- [Un guide complet sur les opérations de manipulation de chaînes de caractères en Javascript] (https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-javascript).