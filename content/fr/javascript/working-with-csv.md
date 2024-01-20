---
title:                "Manipulation des fichiers CSV"
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)

On manipule des CSV (Comma-Separated Values) pour gérer des données tabulaires sous forme de texte simple. C'est utile pour l'import/export avec Excel, des bases de données, ou des apps web.

## How to: (Comment faire : )

Pour lire et écrire des CSV en JavaScript, on peut utiliser la bibliothèque `PapaParse`. Voici comment :

```javascript
// Pour lire un CSV :
Papa.parse(csvString, {
    complete: function(results) {
        console.log(results.data);
    }
});

// Pour écrire un CSV :
var data = [
  ["Column 1", "Column 2"],
  ["foo", "bar"],
  ["baz", "qux"]
];
var csv = Papa.unparse(data);
console.log(csv);
```

Sortie pour la lecture :
```javascript
[
    ["Colonne1", "Colonne2"],
    ["valeur1", "valeur2"],
    ["valeur3", "valeur4"]
]
```

Sortie pour l'écriture :
```
"Column 1","Column 2"
"foo","bar"
"baz","qux"
```

## Deep Dive (Plongée en profondeur)

Historiquement, les CSV sont un format facile à comprendre et à éditer même sans logiciel spécialisé. Plusieurs alternatives existent, comme JSON ou XML, mais CSV reste populaire pour sa simplicité. En JavaScript, lire ou écrire des CSV sans librairie peut être fastidieux à cause des différents cas d'encodage et d'échappement des caractères. D'où l'intérêt d'utiliser `PapaParse` ou des bibliothèques similaires qui s'occupent des détails d'implémentation.

## See Also (Voir aussi)

- PapaParse documentation : [https://www.papaparse.com/docs](https://www.papaparse.com/docs)
- CSV sur MDN : [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/split](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/split) 
- CSV sur W3C : [https://www.w3.org/TR/tabular-data-primer/](https://www.w3.org/TR/tabular-data-primer/)