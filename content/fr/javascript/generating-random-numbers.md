---
title:                "Production de nombres aléatoires"
html_title:           "Javascript: Production de nombres aléatoires"
simple_title:         "Production de nombres aléatoires"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Générer des nombres aléatoires est une pratique courante en programmation qui consiste à créer des nombres au hasard. Les programmeurs ont souvent besoin de nombres aléatoires pour simuler des situations aléatoires ou pour prendre des décisions aléatoires dans leur code.

## Comment faire:

```Javascript
// Utilisation de Math.random() pour générer un nombre aléatoire entre 0 et 1
var randomNumber = Math.random();
console.log(randomNumber); // Exemple de sortie: 0.824536235437

// Utilisation de Math.floor() et Math.random() pour générer un nombre entier aléatoire entre 1 et 10
var randomInteger = Math.floor(Math.random() * 10) + 1; 
console.log(randomInteger); // Exemple de sortie: 7
```

## Plongée profonde:

L'utilisation de nombres aléatoires remonte aux débuts de l'informatique. Auparavant, les utilisateurs devaient saisir leurs propres valeurs aléatoires, mais maintenant, les programmeurs peuvent facilement générer des nombres aléatoires à l'aide de méthodes telles que Math.random(). Cependant, ces nombres ne sont pas complètement aléatoires, ils sont générés à l'aide d'algorithmes qui sont prévisibles. Pour une vraie aléatoire, des sources externes telles que l'horloge interne de l'ordinateur peuvent être utilisées.

## Voir aussi:

- [W3Schools - Math.random()](https://www.w3schools.com/js/js_random.asp)
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Stack Overflow - Generating random whole numbers in JavaScript in a specific range?](https://stackoverflow.com/questions/1527803/generating-random-whole-numbers-in-javascript-in-a-specific-range)