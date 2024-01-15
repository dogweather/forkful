---
title:                "Extraction de sous-chaînes"
html_title:           "Javascript: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous développez des applications web ou des scripts, il est fort probable que vous ayez besoin d'extraire des sous-chaînes de texte à un moment donné. Les sous-chaînes sont des portions de texte qui sont extraites à partir d'une chaîne de caractères plus longue. Cela peut être utile pour manipuler et traiter des données, ou même pour rechercher des informations spécifiques dans une chaîne de texte plus grande.

## Comment faire

Pour extraire des sous-chaînes en Javascript, vous pouvez utiliser la méthode substring() ou slice(). Voici un exemple de code montrant les deux méthodes en action :

```Javascript
// Définition d'une chaîne de caractères
let str = "Bonjour les amis !";

// Utilisation de la méthode substring()
let subStr1 = str.substring(8, 12); // Résultat: les

// Utilisation de la méthode slice()
let subStr2 = str.slice(13, 18); // Résultat: amis

// Affichage des sous-chaînes
console.log(subStr1); // Affiche "les"
console.log(subStr2); // Affiche "amis"
```

Dans cet exemple, nous avons défini une chaîne de caractères, "Bonjour les amis !", et nous avons utilisé les méthodes substring() et slice() pour extraire des sous-chaînes à partir de cette chaîne. Ces méthodes prennent toutes deux en paramètre les indices de début et de fin de la sous-chaîne que nous souhaitons extraire.

## Plongée en profondeur

Il est important de noter que la méthode substring() ne fonctionne pas avec les indices négatifs, tandis que la méthode slice() les accepte. Les deux méthodes peuvent prendre un seul paramètre, qui sera considéré comme l'indice de début, tandis que l'indice de fin sera la fin de la chaîne.

De plus, la méthode substring() retourne une erreur si l'indice de début est supérieur à l'indice de fin, tandis que la méthode slice() inversera automatiquement les indices pour éviter une erreur. Voici un exemple :

```Javascript
let str = "Je suis un développeur";

// Utilisation de la méthode substring()
let subStr1 = str.substring(9,5); // Résultat: erreur

// Utilisation de la méthode slice()
let subStr2 = str.slice(9,5); // Résultat: suis
```

## Voir aussi

Pour plus d'informations sur les méthodes de manipulation de chaînes de caractères en Javascript, vous pouvez consulter les liens suivants :

- [La méthode substring() sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [La méthode slice() sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Les opérations de base sur les chaînes de caractères en Javascript](https://www.w3schools.com/js/js_strings.asp)