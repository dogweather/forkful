---
title:                "Utiliser les expressions régulières"
html_title:           "Javascript: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil très utile pour manipuler des données et rechercher des motifs spécifiques dans du texte. En utilisant des expressions régulières, vous pouvez automatiser certaines tâches qui seraient fastidieuses à faire à la main, ce qui peut vous faire gagner du temps et éviter les erreurs.

## Comment faire

Pour utiliser des expressions régulières en Javascript, vous devez d'abord créer une instance de l'objet RegExp, en spécifiant le motif que vous recherchez. Voici un exemple de code qui montre comment rechercher un numéro de téléphone dans une chaîne de caractères :

```Javascript
const phoneNumber = "Mon numéro de téléphone est le 555-123-4567.";
const pattern = /\d{3}-\d{3}-\d{4}/; // Le motif doit correspondre à un format de numéro de téléphone américain.
const result = pattern.exec(phoneNumber); // Utilisation de la méthode exec pour rechercher le motif dans la chaîne.
console.log(result); // Résultat : ["555-123-4567"]
```

Comme vous pouvez le voir, le résultat est une chaîne de caractères correspondant au motif que nous avons spécifié. Cela peut être très utile si vous devez extraire des informations spécifiques d'un grand ensemble de données.

## Plongée en profondeur

Il existe de nombreux caractères spéciaux que vous pouvez utiliser dans des expressions régulières, tels que "\d" pour représenter un chiffre, "\w" pour représenter un caractère alphanumérique et "\s" pour représenter un espace. Vous pouvez également utiliser des quantificateurs pour spécifier le nombre de fois qu'un motif doit apparaître, par exemple "{3}" pour indiquer qu'il doit y avoir exactement trois occurrences du motif.

Il est également possible d'utiliser des groupes de capture dans des expressions régulières. Ces groupes vous permettent de récupérer des parties spécifiques d'une chaîne correspondant au motif. Par exemple, si nous voulons récupérer séparément le code régional et le numéro de téléphone à partir de notre chaîne précédente, nous pouvons utiliser des parenthèses pour créer des groupes de capture :

```Javascript
const pattern = /(\d{3})-(\d{3}-\d{4})/;
const result = pattern.exec(phoneNumber);
console.log(result); // Résultat : ["555-123-4567", "555", "123-4567"]
```

Vous pouvez ensuite accéder à ces groupes en utilisant les propriétés "index" et "input" de l'objet RegExp.

## Voir aussi

- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_r%C3%A9guli%C3%A8res)
- [W3Schools - JavaScript Regular Expressions](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [RegExp Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript)