---
title:                "Javascript: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Trouver la longueur d'une chaîne de caractères peut sembler être une tâche simple en programmation, mais cela peut être très utile pour de nombreuses raisons. Par exemple, trouver la longueur d'un mot peut aider à vérifier si celui-ci respecte une limite de caractères, ou encore pour afficher un message d'erreur si le mot est trop long. Dans cet article, nous allons voir comment trouver la longueur d'une chaîne de caractères en utilisant Javascript.

## Comment faire

Pour trouver la longueur d'une chaîne en Javascript, nous pouvons utiliser la méthode `length` qui est disponible pour tous les objets de type chaîne. Cette méthode retourne le nombre de caractères dans une chaîne.

```Javascript
let chaine = "Bonjour le monde";
console.log(chaine.length); // Output: 17
```

Ici, nous avons déclaré une variable `chaine` avec une chaîne de caractères et nous avons utilisé la méthode `length` pour obtenir la longueur de la chaîne et l'afficher dans la console.

Nous pouvons également utiliser cette méthode pour vérifier la longueur d'une variable ou d'un argument passé à une fonction:

```Javascript
function longueur(chaine) {
  return chaine.length;
}

console.log(longueur("Bonjour")); // Output: 7
```

Dans cet exemple, nous avons déclaré une fonction qui prend une chaîne en paramètre et retourne la longueur de cette chaîne en utilisant la méthode `length`.

## Plongée en profondeur

Il est important de noter que la longueur renvoyée par la méthode `length` correspond au nombre de caractères et non au nombre de mots. Par exemple, si notre chaîne contient un espace, celui-ci sera également compté comme un caractère.

De plus, la méthode `length` ne fonctionne que pour les chaînes de caractères et pas pour les autres types de données. Par conséquent, si vous essayez de l'utiliser sur un nombre ou un booléen, vous obtiendrez une erreur.

Enfin, il est également possible d'utiliser la méthode `length` sur une chaîne vide, ce qui retournera simplement 0.

## Voir aussi

- [Documentation sur la méthode `length` en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Différentes façons de trouver la longueur d'une chaîne en Javascript](https://www.w3resource.com/javascript-exercises/javascript-string-exercise-1.php)
- [Explications détaillées sur les chaînes en Javascript](https://codeburst.io/javascript-strings-everything-you-need-to-know-53254b651a58)