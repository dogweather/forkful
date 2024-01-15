---
title:                "La concaténation des chaînes de caractères"
html_title:           "Javascript: La concaténation des chaînes de caractères"
simple_title:         "La concaténation des chaînes de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi Concaténer des Chaînes de Caractères en Javascript?

Si vous avez besoin de combiner plusieurs morceaux de textes différents pour afficher une information complète, vous devrez concaténer des chaînes de caractères en Javascript. Cela vous permet de créer une seule chaîne de caractères qui contient toutes les informations dont vous avez besoin.

## Comment Faire

```Javascript
let firstName = "Jean";
let lastName = "Dupont";
let fullName = firstName + " " + lastName; // "Jean Dupont"
console.log(fullName); // Affiche "Jean Dupont"
```

Il existe plusieurs façons de concaténer des chaînes de caractères en Javascript. Vous pouvez utiliser l'opérateur de concaténation '+' pour ajouter des chaînes de caractères, ou la méthode 'concat()' pour concaténer plusieurs chaînes de caractères à la fois. Vous pouvez également utiliser des templates literals (``) pour concaténer des variables et du texte sans utiliser d'opérateur.

## Plongée Profonde

En Javascript, les chaînes de caractères sont des objets immuables. Cela signifie que lorsqu'une chaîne de caractères est créée, elle ne peut pas être modifiée. Lorsque vous concaténez des chaînes de caractères, vous créez en fait une toute nouvelle chaîne de caractères, sans modifier les chaînes de caractères d'origine.

De plus, la concaténation peut être une opération coûteuse en termes de performances, car elle implique la création et l'allocation de nouvelles chaînes de caractères en mémoire. Dans certaines situations, il peut être plus efficace d'utiliser la méthode 'join()' pour concaténer plusieurs chaînes de caractères contenues dans un tableau.

## Voir Aussi

- [MDN - Concaténation de chaînes de caractères en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN - Templates literals en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Template_literals)
- [MDN - Performances en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Performance/Optimizing_Profiles)