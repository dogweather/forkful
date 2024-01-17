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

# De quoi s'agit-il et pourquoi le faisons-nous?

L'extraction de sous-chaînes, également appelée découpage de chaîne de caractères, est une méthode couramment utilisée en programmation pour extraire certaines parties spécifiques d'une chaîne de caractères. Cela peut être utile pour obtenir uniquement les informations nécessaires à partir d'une chaîne plus longue, ou pour manipuler des données plus facilement. Les développeurs utilisent cette technique pour simplifier leur code et rendre leur programme plus efficace.

## Comment faire :

Voici un exemple de code en Javascript montrant comment extraire une sous-chaîne à partir d'une chaîne donnée :

```Javascript
let str = "Bonjour tout le monde!";
let sub = str.substring(8, 12); 
console.log(sub); // Output: tout
```

Dans cet exemple, nous avons une chaîne de caractères contenant "Bonjour tout le monde!" et nous utilisons la méthode `substring()` pour extraire une sous-chaîne de la 8ème à la 12ème position. Le résultat est une nouvelle chaîne "tout". Vous pouvez également spécifier une seule position, auquel cas la méthode extraira la sous-chaîne à partir de cette position jusqu'à la fin de la chaîne d'origine.

Vous pouvez également utiliser la méthode `slice()` pour le même effet :

```Javascript
let str = "Bonjour tout le monde!";
let sub = str.slice(8, 12); 
console.log(sub); // Output: tout
```

La principale différence entre `substring()` et `slice()` est que `substring()` ne permet pas de spécifier un index négatif, alors que `slice()` le prend en charge.

## Examen approfondi :

Historiquement, la méthode `substring()` a été introduite dès les premières versions de Javascript pour offrir une alternative à la méthode `slice()`, qui existait déjà en tant que fonction de traitement de chaînes de caractères en Javascript. La méthode `substring()` est plus restrictive en termes de manipulation de chaînes de caractères, mais elle est plus largement utilisée pour son comportement prévisible.

Bien que `substring()` et `slice()` soient des méthodes utiles pour extraire des sous-chaînes, les développeurs modernes favorisent souvent d'autres méthodes telles que `split()` ou `match()` qui permettent de mieux prendre en charge les expressions régulières et les modifications dynamiques à une chaîne de caractères.

Si vous souhaitez en savoir plus sur les différentes méthodes de traitement des chaînes de caractères en Javascript, jetez un œil à [ce guide complet sur les fonctions natives de traitement des chaînes en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String).

## À voir également :

Si vous voulez en savoir plus sur la manipulation de chaînes de caractères en Javascript, ces ressources peuvent être utiles :

- [Documentation officielle sur les fonctions natives de traitement des chaînes en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String)
- [Guide complet sur la manipulation des chaînes de caractères en Javascript](https://www.tutorialspoint.com/javascript/javascript_string_methods.htm)
- [Tutoriel interactif sur l'utilisation de la méthode `substring()` en Javascript](https://www.w3schools.com/jsref/jsref_substr.asp)