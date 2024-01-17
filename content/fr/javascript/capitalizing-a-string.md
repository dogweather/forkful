---
title:                "Capitaliser une chaîne de caractères"
html_title:           "Javascript: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que la mise en majuscule et pourquoi les programmeurs la font-ils?

La mise en majuscule est le fait de transformer toutes les lettres d'une chaîne de caractères en lettres majuscules. Les programmeurs le font souvent pour des raisons de lisibilité et de formatage dans leur code.

# Comment faire:

```Javascript
let string = "salut tout le monde";
console.log(string.toUpperCase());
```
Sortie: SALUT TOUT LE MONDE

```Javascript
let phrase = "voici une phrase bien longue";
console.log(phrase.toUpperCase());
```
Sortie: VOICI UNE PHRASE BIEN LONGUE

# Approfondissement:

La mise en majuscule vient de la convention d'écriture appelée "CamelCase", où les mots sont séparés par des majuscules au lieu d'espaces ou de tirets. Il existe également une méthode alternative pour mettre une chaîne de caractères en majuscules en utilisant la méthode .replace() en combinaison avec des expressions régulières.

# Voir aussi:

- [MDN - toUpperCase](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [W3Schools - JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)