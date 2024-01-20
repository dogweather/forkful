---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ? 
Trouver la longueur d'une chaîne de caractères signifie déterminer le nombre de caractères dans cette chaîne. Les programmeurs le font pour manipuler et contrôler les données textuelles efficacement.

## Comment faire:
Pour trouver la longueur d'une chaîne de caractères en Javascript, vous pouvez utiliser la propriété `length`. Voici un exemple:

```Javascript
var maChaine = "Salut tout le monde!";
console.log(maChaine.length);  //Affiche 20
```

Dans cet exemple, `maChaine.length` renvoie la longueur de maChaine, qui est de 20 caractères.

## Plongée en Profondeur
La méthode `length` a été introduite dans JavaScript 1.0, et est depuis lors un outil efficace pour mesurer la longueur des chaînes. Vous pouvez aussi utiliser la méthode `split` et `join` pour calculer la longueur d'une chaîne, mais la propriété `length` offre une meilleure performance.

La propriété `length` renvoie le nombre de points de code UTF-16 dans la chaîne. C'est important de savoir que des caractères comme les émojis sont considérés comme deux caractères.

```Javascript
var maChaine = "Salut 👋";
console.log(maChaine.length);  //Affiche 7
```

Ici, l'émoji 👋 est compté comme deux caractères.

## Voir Aussi
Pour plus d'informations sur les chaînes de caractères en Javascript, consultez ces sources:

- [Chaîne de caractères - JavaScript | MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String)
- [JavaScript String Length Property - w3schools](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [Understanding JavaScript’s ‘unicode problem’ - Mathias Bynens](https://mathiasbynens.be/notes/javascript-unicode)