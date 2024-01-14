---
title:                "Javascript: Majuscule d'une chaîne de caractères"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Il est souvent nécessaire de capitaliser une chaîne de caractères en programmant en Javascript. Cela peut être utile pour mettre en évidence certains mots dans une phrase, pour uniformiser le style de texte ou pour de nombreuses autres raisons. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant Javascript et pourquoi nous pourrions le faire.

## Comment faire

Pour capitaliser une chaîne de caractères en Javascript, nous pouvons utiliser la méthode `toUpperCase()`. Cette méthode convertit tous les caractères d'une chaîne en lettres majuscules. Voyons un exemple concret :

```Javascript
let phrase = "programmation en javascript";

let phraseCapitalize = phrase.toUpperCase();

console.log(phraseCapitalize);
// affiche "PROGRAMMATION EN JAVASCRIPT"
```

Dans cet exemple, nous créons une variable `phrase` avec la valeur "programmation en javascript". Ensuite, nous utilisons la méthode `toUpperCase()` pour capitaliser la chaîne et enfin nous affichons le résultat dans la console.

Vous pouvez également capitaliser uniquement le premier caractère d'une chaîne en utilisant la méthode `charAt()` et la concaténation de chaînes. Voici un exemple :

```Javascript
let str = "ma chaîne de caractères";
let strCapitalize = str.charAt(0).toUpperCase() + str.slice(1);

console.log(strCapitalize);
// affiche "Ma chaîne de caractères"
```

Dans cet exemple, nous utilisons la méthode `charAt()` pour extraire le premier caractère de la chaîne, puis nous utilisons la méthode `toUpperCase()` pour le convertir en majuscule. Enfin, nous concaténons cette lettre avec le reste de la chaîne en utilisant la méthode `slice()`.

## Plongée en profondeur

Maintenant que nous savons comment capitaliser une chaîne de caractères en Javascript, voyons pourquoi cela peut être utile. Comme mentionné précédemment, la capitale peut être utilisée pour mettre en évidence certains mots dans une phrase, mais elle peut également être nécessaire pour des raisons de style ou de formatage.

Un cas d'utilisation courant est lorsque nous souhaitons afficher le nom et le prénom d'une personne avec la première lettre de chaque mot en majuscule. Nous pouvons utiliser la méthode `split()` pour séparer le nom et le prénom en deux chaînes, puis capitaliser chacune d'entre elles avant de les concaténer à nouveau. Voyons un exemple :

```Javascript
let fullName = "jean dupont";
let names = fullName.split(" ");
let firstNameCapitalize = names[0].charAt(0).toUpperCase() + names[0].slice(1);
let lastNameCapitalize = names[1].charAt(0).toUpperCase() + names[1].slice(1);
let capitalizedFullName = firstNameCapitalize + " " + lastNameCapitalize;

console.log(capitalizedFullName);
// affiche "Jean Dupont"
```

Nous utilisons la méthode `split()` pour séparer la chaîne `fullName` en deux chaînes : "jean" et "dupont". Ensuite, nous appliquons la méthode `toUpperCase()` à la première lettre de chaque chaîne et concaténons les résultats pour obtenir "Jean Dupont".

Vous pouvez également utiliser cette méthode pour capitaliser les initiales d'un acronyme ou pour mettre en forme une date en majuscules.

## Voir aussi

- [La méthode `toUpperCase()` sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toUpperCase)
- [La méthode `charAt()` sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/charAt)
- [La méthode `slice()` sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)