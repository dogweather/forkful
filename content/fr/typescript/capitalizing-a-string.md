---
title:                "TypeScript: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsqu'on travaille avec des chaînes de caractères en TypeScript, il peut être utile de pouvoir les convertir en majuscules ou en minuscules selon nos besoins. Cela peut être nécessaire lors de la comparaison de chaînes de caractères ou pour une présentation cohérente des données dans une application. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en TypeScript.

## Comment procéder

La méthode la plus simple pour capitaliser une chaîne de caractères en TypeScript est d'utiliser la méthode "toUpperCase()" de JavaScript. Voici un exemple de code qui utilise cette méthode :

```TypeScript
let string = "bonjour tout le monde";
console.log(string.toUpperCase());
```

Cela va afficher la chaîne de caractères en majuscules : "BONJOUR TOUT LE MONDE".

## Plongée en profondeur

Il est important de noter que la méthode "toUpperCase()" ne modifie pas la chaîne de caractères d'origine, mais retourne une nouvelle chaîne de caractères en majuscules. Si vous souhaitez modifier la chaîne de caractères d'origine, vous pouvez utiliser la méthode "charAt()" pour accéder et modifier chaque caractère individuellement.

Il existe également d'autres méthodes pour capitaliser une chaîne de caractères selon différents critères, telles que la fonction "capitalize()" de la librairie Lodash ou encore la méthode "replace()" de JavaScript. Il est important de choisir la méthode qui convient le mieux à votre cas d'utilisation.

## Voir aussi

- [Documentation sur la méthode "toUpperCase()" de JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Documentation sur la méthode "charAt()" de JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [Documentation sur la fonction "capitalize()" de Lodash](https://lodash.com/docs/4.17.15#capitalize)
- [Documentation sur la méthode "replace()" de JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/replace)