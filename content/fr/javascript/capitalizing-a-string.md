---
title:                "Javascript: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Le fait de capitaliser une chaîne de caractères peut être très utile lors de la manipulation de données dans un programme Javascript. Cela permet de mettre en évidence certains mots ou de formater correctement des informations avant de les afficher à l'utilisateur.

## Comment faire

Pour capitaliser une chaîne de caractères en Javascript, il existe plusieurs méthodes possibles. En voici deux exemples :

```Javascript
// Méthode 1 : Utilisation de la méthode toUpperCase()
let string = "javascript programming";
let capitalizedString = string.toUpperCase();
console.log(capitalizedString);
// Output: JAVASCRIPT PROGRAMMING

// Méthode 2 : Utilisation de la méthode charAt() et toUpperCase()
let string = "javascript programming";
let capitalizedString = string.charAt(0).toUpperCase() + string.slice(1);
console.log(capitalizedString);
// Output: Javascript programming
```

Dans le premier exemple, nous utilisons la méthode toUpperCase() qui permet de convertir l'ensemble de la chaîne de caractères en majuscules. Dans le second exemple, nous utilisons la méthode charAt() pour sélectionner la première lettre de la chaîne et la méthode toUpperCase() pour la convertir en majuscule. Ensuite, nous ajoutons le reste de la chaîne à l'aide de la méthode slice() qui permet de sélectionner une partie de la chaîne à partir d'un index donné.

Il existe également d'autres méthodes telles que toLocaleUpperCase() qui permet de prendre en compte la langue et la culture de l'utilisateur.

## Plongée en profondeur

La méthode la plus simple pour capitaliser une chaîne de caractères est d'utiliser toUpperCase(). Cependant, cette méthode ne prend pas en compte les accents ou les caractères spéciaux. Ce qui signifie que si votre chaîne de caractères contient des lettres avec des accents, elles ne seront pas capitalisées. C'est pour cela que la méthode toLocaleUpperCase() est plus recommandée, car elle tient compte de la langue et de la culture de l'utilisateur.

Il est également important de noter que la méthode toUpperCase() ne modifie pas la chaîne originale, elle renvoie une nouvelle chaîne de caractères modifiée. Si vous souhaitez modifier directement la chaîne d'origine, vous pouvez utiliser la méthode charAt() et toUpperCase() comme dans notre deuxième exemple.

## Voir aussi

Pour plus d'informations sur les manipulations de chaînes de caractères en Javascript, vous pouvez consulter les liens suivants :

- [Documentation JavaScript sur les chaînes de caractères](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Différentes méthodes de manipulation des chaînes de caractères en JavaScript](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-javascript)

N'hésitez pas à expérimenter et à trouver la méthode qui convient le mieux à votre besoin !