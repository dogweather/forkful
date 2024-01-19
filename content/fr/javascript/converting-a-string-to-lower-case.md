---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Convertir une chaîne en minuscule est le processus par lequel toutes les lettres majuscules dans une chaîne de caractères sont converties en minuscules. Les programmeurs le font pour effectuer des comparaisons sans tenir compte de la distinction entre majuscules et minuscules.

## Comment faire :

Voici comment on convertit une chaîne en minuscule en Javascript:
```Javascript
let maChaine = "Bonjour Le Monde!";
let chaineEnMinuscules = maChaine.toLowerCase();
console.log(chaineEnMinuscules);
```

L'exemple ci-dessus affichera "bonjour le monde!" dans la console.

## Plongée en profondeur:

Historiquement, dans les premiers jours de l'informatique, les systèmes étaient sensibles à la casse. Cela a finalement conduit à la nécessité de fonctions pour manipuler la casse des chaînes.

Il y a quelques alternatives à l'utilisation de `toLowerCase()`. Vous pouvez parcourir chaque caractère de la chaîne, vérifier si c'est une lettre majuscule et, si c'est le cas, le convertir en minuscule.

 ```Javascript
let maChaine = "Bonjour Le Monde!";
let chaineEnMinuscules = '';
for(let i = 0; i < maChaine.length; i++){
     chaineEnMinuscules += maChaine[i].toLowerCase();
}
 console.log(chaineEnMinuscules);
```

Cependant, utiliser `toLowerCase()` est généralement plus efficace et plus propre dans le code.

## Voir aussi:

- La méthode `toUpperCase()`, qui fait l'inverse, convertit tous les caractères de la chaîne en majuscules. 
- Documentation JavaScript MDN sur [toLowerCase()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase) 
- Stack Overflow: [How to convert string to lowercase in Javascript](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)