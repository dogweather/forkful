---
title:                "TypeScript: La génération de nombres aléatoires"
simple_title:         "La génération de nombres aléatoires"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une compétence essentielle pour tout programmeur, car elle peut être utilisée dans une variété de scénarios tels que les jeux, les simulations, ou encore les tests de logiciels. Cela peut également ajouter un élément de surprise et de dynamisme à vos projets.

## Comment Faire

```TypeScript
// Utilisation de la fonction Math.random() pour générer un nombre aléatoire entre 0 et 1
let randomNum: number = Math.random();
console.log(randomNum); // Exemple de sortie : 0.54873891936

// Utilisation de Math.floor() pour obtenir un nombre entier
let randomInt: number = Math.floor(Math.random() * 10); // Génère un nombre aléatoire entre 0 et 9
console.log(randomInt); // Exemple de sortie : 5

// Utilisation de Math.ceil() pour obtenir un nombre entier supérieur
let randomCeil: number = Math.ceil(Math.random() * 10); // Génère un nombre aléatoire entre 1 et 10
console.log(randomCeil); // Exemple de sortie : 7

// Utilisation de Math.round() pour obtenir un nombre entier arrondi
let randomRound: number = Math.round(Math.random() * 10); // Génère un nombre aléatoire entre 0 et 10
console.log(randomRound); // Exemple de sortie : 8
```

Vous pouvez également utiliser ces fonctions pour générer des nombres aléatoires dans des plages spécifiques en utilisant des opérateurs mathématiques appropriés.

## Plongée Profonde

Bien que la génération de nombres aléatoires puisse sembler simple, il y a en réalité beaucoup de mathématiques et de logique impliquées pour garantir une distribution équitable des nombres. Le générateur de nombres aléatoires natif de TypeScript utilise l'algorithme du générateur linéaire congruentiel (GLC), qui utilise des opérations mathématiques telles que la multiplication, l'addition et la division pour générer des nombres aléatoires. Il est important de comprendre que ces générateurs produisent des nombres pseudo-aléatoires et ne doivent pas être utilisés pour des applications sensibles à la sécurité.

## Voir Aussi

- [Documentation officielle de TypeScript sur la génération de nombres aléatoires](https://www.typescriptlang.org/docs/handbook/release-notes/overview.html#random-number-generation)
- [Article sur les algorithmes de génération de nombres aléatoires en programmation](https://www.tomshardware.com/reviews/random-number-generation,633.html)
- [Site Web expliquant la différence entre les nombres aléatoires et les nombres pseudo-aléatoires](https://www.random.org/randomness/)

**Merci d'avoir lu !**