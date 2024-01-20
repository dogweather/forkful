---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Extraire des sous-chaînes, c'est récupérer des parties spécifiques d'une chaîne de caractères. Les programmeurs le font pour manipuler et analyser les données avec plus de précision.

## Comment faire:

En JavaScript, on peut utiliser `substring()`, `substr()` ou `slice()` pour extraire une sous-chaîne. Voici quelques exemples:

```javascript
// substring(start, end)
let str = "Bonjour le monde!";
console.log(str.substring(0, 7));  // Résultat: "Bonjour"

//substr(start, length)
console.log(str.substr(8, 5));  // Résultat: "le mo"
  
//slice(start, end) 
console.log(str.slice(13));  // Résultat: "monde!"
```
## Plongée profonde

Historiquement, la méthode `substring()` existait en Javascript depuis ses débuts. `substr()` et `slice() `ont été ajoutées plus tard pour plus de flexibilité. Cependant, la méthode `substr()` est maintenant considérée comme obsolète et non conseillée pour une utilisation future.

Une alternative à l'extraction de sous-chaînes est l'utilisation d'expressions régulières. Cependant, pour des cas simples, `substring()` et `slice()` sont souvent plus claires et plus faciles à comprendre.

Les méthodes `substring()` et `slice()` se comportent de manière similaire mais diffèrent légèrement lorsqu'elles traitent des indices négatifs. `substring()` ne permet pas d'indices négatifs et les traite comme 0. `slice()`, en revanche, permet d'indices négatifs où -1 fait référence au dernier caractère de la chaîne.

## Voir aussi

Pour plus d'informations, consultez les liens suivants:
- [Function.prototype.substring() MDN Docs](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/substring)
- [Function.prototype.substr() MDN Docs](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/substr)
- [Function.prototype.slice() MDN Docs](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)
- [Utiliser des expressions régulières en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions)