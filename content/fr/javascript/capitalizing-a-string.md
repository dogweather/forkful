---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
simple_title:         "Mettre une chaîne de caractères en majuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi?)
Capitaliser une chaîne, c'est transformer toutes ses lettres en majuscules. Les développeurs font ça pour uniformiser les données, pour l'importance visuelle ou pour respecter des conventions de nommage.

## How to: (Comment faire : )
En JavaScript, pour capitaliser une chaîne, on utilise souvent la méthode `toUpperCase()` :

```Javascript
let phrase = "bonjour le monde!";
let phraseCapitale = phrase.toUpperCase();

console.log(phraseCapitale); // Affiche "BONJOUR LE MONDE!"
```

Et pour changer le premier caractère seul en majuscule :

```Javascript
let mot = "paris";
let motCapitalise = mot.charAt(0).toUpperCase() + mot.slice(1);

console.log(motCapitalise); // Affiche "Paris"
```

## Deep Dive (Plongée en profondeur)
Historiquement, capitaliser une chaîne n'était pas aussi direct qu'aujourd'hui. Les premières versions de JavaScript ne disposaient pas de méthodes natives pour changer la casse. Les développeurs utilisaient des boucles pour transformer chaque lettre.

La méthode `toUpperCase()` est la plus directe, mais pas la seule. Pour seulement capitaliser la première lettre, l'exemple précédent est utile. Ceci est aussi pertinent si vous travaillez avec des systèmes qui exigent certaines conventions, comme un prénom toujours débutant par une majuscule.

Si on a besoin de plus de flexibilité, par exemple changer la casse dans différents contextes (comme camelCase ou PascalCase), on doit implémenter des fonctions personnalisées. Voici un aperçu :

```Javascript
function toPascalCase(str) {
    return str
        .match(/[a-z]+/gi)
        .map(word => word.charAt(0).toUpperCase() + word.substr(1).toLowerCase())
        .join('');
}

console.log(toPascalCase("envie d'apprendre")); // Affiche "EnvieDApprendre"
```

Cette fonction recherche les mots, capitalise la première lettre de chacun, puis les assemble.

## See Also (Voir aussi)
- MDN Web Docs pour `toUpperCase()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- MDN Web Docs pour `charAt()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
- W3Schools sur JavaScript String Methods: https://www.w3schools.com/js/js_string_methods.asp
