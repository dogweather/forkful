---
title:                "Javascript: Trouver la longueur d'une chaîne"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche de la longueur d'une chaîne de caractères peut sembler être une action simple et banale en programmation. Pourtant, connaître la longueur d'une chaîne de caractères peut être utile pour réaliser diverses tâches telles que valider des entrées utilisateur, manipuler des données ou afficher du contenu dynamique. Dans cet article, nous allons examiner les différentes façons de trouver la longueur d'une chaîne de caractères en utilisant JavaScript.

## Comment faire

L'une des façons les plus simples de trouver la longueur d'une chaîne de caractères est d'utiliser la méthode `length` sur un objet de chaîne. Cette méthode renvoie le nombre de caractères présents dans la chaîne de caractères. Voyons un exemple concret :

```javascript
var maChaine = "Ceci est une phrase";
console.log(maChaine.length); // Sortie : 18
```

Dans cet exemple, nous utilisons la méthode `length` sur la variable `maChaine` pour obtenir la longueur de la chaîne de caractères. Cette méthode peut également être utilisée sur des chaînes de caractères vides, qui renverront une valeur de 0.

En plus de la méthode `length`, nous pouvons également utiliser la propriété `length` sur un objet de chaîne de caractères pour obtenir le même résultat :

```javascript
var maChaine = "Ceci est une phrase";
console.log(maChaine.length); // Sortie : 18
```

Il est important de noter que la longueur renvoyée par ces méthodes et propriétés représente le nombre de caractères, et non le nombre de mots.

## Plongée en profondeur

Mais comment fonctionnent ces méthodes pour trouver la longueur d'une chaîne de caractères ? En réalité, la propriété `length` est calculée en interne par le moteur JavaScript en parcourant chaque caractère de la chaîne et en comptant leur nombre. Il s'agit donc d'une opération relativement coûteuse en termes de performances. Il est donc préférable d'utiliser ces méthodes avec parcimonie et d'autres alternatives existent pour obtenir la longueur d'une chaîne de caractères, telles que l'utilisation de boucles ou de méthodes de manipulation de chaînes de caractères.

## Voir aussi

- [MDN - Méthode length de String](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length)
- [Stack Overflow - Find the length of a string in JavaScript](https://stackoverflow.com/questions/286218/remove-blank-attributes-when-converting-string-to-json)