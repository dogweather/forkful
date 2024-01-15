---
title:                "Trouver la longueur d'une chaîne de caractères."
html_title:           "Javascript: Trouver la longueur d'une chaîne de caractères."
simple_title:         "Trouver la longueur d'une chaîne de caractères."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Comme vous le savez peut-être déjà, une chaîne de caractères est un type de donnée très commun en Javascript. Il est donc utile de savoir comment trouver la longueur d'une chaîne de caractères. Cela peut être utile pour de nombreuses tâches, telles que la validation de la saisie d'un utilisateur ou la manipulation de données.

## Comment Faire

Pour trouver la longueur d'une chaîne de caractères en Javascript, nous pouvons utiliser la méthode `length`. Elle retourne la taille de la chaîne sous forme d'un nombre entier.

```Javascript
let str = "Bonjour tout le monde!";

console.log(str.length); // Output: 21 
```

Comme vous pouvez le voir dans l'exemple ci-dessus, `str.length` renvoie la longueur de la chaîne `str`. Il est important de noter que la méthode `length` compte les espaces et les caractères spéciaux également.

Nous pouvons également utiliser cette méthode pour vérifier si une chaîne a une longueur spécifique en la comparant à un nombre.

```Javascript
let str = "Salut";

if(str.length === 5){
    console.log("La chaîne a une longueur de 5 caractères."); // Output: La chaîne a une longueur de 5 caractères.
} else {
    console.log("La chaîne n'a pas une longueur de 5 caractères.");
}
```

## Deep Dive

Il est important de noter que `length` n'est pas une propriété, mais une méthode. Cela signifie que nous devons ajouter des parenthèses à la fin du nom pour l'exécuter. Si nous utilisons simplement `str.length` sans les parenthèses, il nous renverra la définition de la méthode, plutôt que le nombre de caractères de la chaîne.

De plus, la longueur d'une chaîne peut être influencée par la langue et les accents utilisés. Par exemple, en français, la chaîne "é" est considérée comme un caractère, tandis qu'en anglais, elle est comptée comme deux caractères (équivalent à "e").

## Voir Aussi

- [La documentation officielle de Javascript sur la méthode `length`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length)
- [Un tutoriel sur les chaînes de caractères en Javascript](https://www.w3schools.com/js/js_strings.asp)