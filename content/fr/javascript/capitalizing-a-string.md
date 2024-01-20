---
title:                "Mettre une chaîne en majuscules"
html_title:           "Javascript: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi ?

Mettre en majuscule une chaîne de caractères signifie convertir les premières lettres de chaque mot en majuscules. Les développeurs le font pour améliorer la lisibilité et normaliser les données.

## Comment faire :

Voici un exemple simple pour mettre en majuscule la première lettre d'une chaîne de caractères dans JavaScript.
```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}
```
Utilisons cette fonction :
```javascript
let str = 'bonjour, les programmeurs!';
console.log(capitalizeFirstLetter(str)); 
// Résultat: 'Bonjour, les programmeurs!'
```
## Immersion profonde

Historiquement, en JavaScript, on utilisait la technique du splice comme dans l'exemple ci-dessus pour capitaliser les chaînes. Mais l'ES6 a introduit les modèles de chaînes qui facilitent encore plus ce processus.
Voici une alternative ES6 :
```javascript
const capitalizeFirstLetterES6 = string => 
`${string.charAt(0).toUpperCase()}${string.slice(1)}`;
```
Il existe également d'autres solutions comme utiliser `split` pour diviser la chaîne en mots, puis capitaliser chaque mot individuellement, surtout lorsque vous travaillez avec une phrase.

Il est important de noter que `toUpperCase` est une fonction intégrée en JavaScript, elle n'a donc pas besoin d'une implémentation manuelle.

## Voir aussi 

1. La documentation officielle de JavaScript sur MDN pour `toUpperCase`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toUpperCase
2. La documentation es6: https://exploringjs.com/es6/ch_template-literals.html
3. Une discussion StackOverflow sur diverses manières de capitaliser les chaînes en JavaScript: https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript