---
title:                "TypeScript: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation, que ce soit pour manipuler des données ou pour valider une entrée utilisateur. Dans cet article, nous allons discuter de la façon de le faire en TypeScript et pourquoi c'est important pour votre code.

## Comment faire

Pour trouver la longueur d'une chaîne en TypeScript, nous utilisons la méthode `length` sur l'objet de chaîne de caractères. Voici un exemple de code:

```
TypeScript
let chaine = "Bonjour le monde";
console.log(chaine.length); //affichera 17
```

Nous pouvons également utiliser cette méthode avec des chaînes de caractères définies avec des guillemets simples ou avec des variables contenant des chaînes de caractères.

```
TypeScript
let nom = 'Jean';
console.log(nom.length); //affichera 4
```

Cela fonctionne également avec des chaînes de caractères multi-lignes:

```
TypeScript
let multiLigne = `
Première ligne
Deuxième ligne
`;
console.log(multiLigne.length); //affichera 28
```

Il est important de noter que la méthode `length` compte également les espaces et les caractères spéciaux dans la chaîne.

## Deep Dive

Maintenant, plongeons un peu plus en profondeur pour comprendre pourquoi la méthode `length` fonctionne de cette façon en TypeScript. En réalité, cette méthode n'est pas spécifique à TypeScript, elle fait partie du prototype de l'objet string en JavaScript. Elle est donc accessible pour tous les langages qui reposent sur la syntaxe JavaScript, y compris TypeScript.

La méthode `length` envoie une requête à l'objet string, qui renvoie la longueur de la chaîne en tant que nombre. C'est pourquoi nous pouvons appeler la méthode sans utiliser de parenthèses, car elle ne prend pas de paramètres.

Vous vous demandez peut-être pourquoi nous utilisons la méthode `length` lorsque nous pourrions simplement compter le nombre de caractères dans une chaîne à l'aide de la propriété `length` de l'objet. Cela est dû au fait que la méthode `length` est plus flexible et prend également en compte les caractères Unicode et les caractères spéciaux, tandis que la propriété `length` ne compte que les caractères ASCII ordinaires.

En utilisant la méthode `length`, nous pouvons donc avoir une mesure plus précise de la longueur d'une chaîne de caractères, en tenant compte de tous les caractères qui la composent.

## Voir aussi

- [Documentation officielle sur la méthode `length` en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length)
- [Documentation officielle sur les chaînes de caractères en TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Un article sur la manipulation des chaînes de caractères en TypeScript](https://dev.to/amejiarosario/typescript---string-manipulation-8b8)

Merci d'avoir lu cet article sur la méthode `length` en TypeScript. J'espère que cela vous a aidé à mieux comprendre son fonctionnement et pourquoi elle est importante pour votre code. N'hésitez pas à consulter les liens ci-dessus pour en savoir plus sur les chaînes de caractères en JavaScript et en TypeScript. Happy coding!