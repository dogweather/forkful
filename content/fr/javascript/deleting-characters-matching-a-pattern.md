---
title:                "Javascript: Supprimer des caractères correspondant à un motif"
simple_title:         "Supprimer des caractères correspondant à un motif"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être nécessaire lors de la manipulation de chaînes de caractères en Javascript. Par exemple, si vous souhaitez supprimer tous les espaces d'une chaîne de caractères pour un traitement ultérieur, vous devrez utiliser une méthode pour supprimer des caractères correspondant à l'espace dans la chaîne.

## Comment faire

Pour supprimer des caractères correspondant à un modèle dans une chaîne de caractères en Javascript, vous pouvez utiliser la méthode .replace(). Cette méthode prend deux arguments : le premier est le modèle à rechercher, et le deuxième est la chaîne de caractères qui sera utilisée pour remplacer les caractères correspondants. Voici un exemple de code :

```Javascript
let str = "Bonjour tout le monde!";
let newStr = str.replace(/o/g, "a");
console.log(newStr); // Bjanaur taut la mande!
```

Dans cet exemple, nous utilisons la méthode .replace() pour remplacer tous les "o" par des "a", ce qui nous donne le résultat "Bjanaur taut la mande!".

Il est également possible d'utiliser une expression régulière comme modèle. Par exemple, pour supprimer tous les caractères de ponctuation dans une chaîne de caractères, vous pouvez utiliser l'expression régulière /[^\w\s]/g. Voici un exemple de code :

```Javascript
let str = "Bonjour! Comment ça va?";
let newStr = str.replace(/[^\w\s]/g, "");
console.log(newStr); // Bonjour Comment ça va
```

Dans ce cas, nous utilisons l'expression régulière /[^\w\s]/g pour rechercher tous les caractères qui ne sont ni des lettres ni des espaces, et nous les supprimons de la chaîne en les remplaçant par une chaîne vide.

## Plongée en profondeur

Lors de l'utilisation de la méthode .replace(), il est important de noter que celle-ci ne modifie pas la chaîne originale, mais plutôt renvoie une nouvelle chaîne avec les modifications appliquées. Si vous souhaitez modifier la chaîne originale, vous devrez affecter le résultat de la méthode à la variable contenant la chaîne d'origine.

De plus, il est possible de passer une fonction en tant que deuxième argument de la méthode .replace(). Cette fonction prendra en paramètres le modèle correspondant et l'index de la correspondance, et renvoie la chaîne qui sera utilisée pour remplacer la correspondance. Cela peut être utile pour effectuer des modifications plus complexes en utilisant la logique de la fonction. Voici un exemple de code :

```Javascript
let str = "Mon numéro de téléphone est 555-123-4567";
let newStr = str.replace(/\d{3}-\d{3}-\d{4}/g, function(match, index) {
  return match.replace(/-/g, "/")
});
console.log(newStr); // Mon numéro de téléphone est 555/123/4567
```

Dans cet exemple, nous utilisons une expression régulière pour trouver le numéro de téléphone dans la chaîne et nous utilisons la fonction de remplacement pour remplacer tous les tirets par des slashes. Cela peut être utile pour formater les données de manière spécifique.

## Voir aussi

- Documentation de la méthode .replace() sur MDN: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace
- Expression régulière en Javascript sur W3Schools: https://www.w3schools.com/jsref/jsref_obj_regexp.asp