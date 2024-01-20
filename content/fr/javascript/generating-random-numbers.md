---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi et Qu'est-ce que c'est?

Générer des nombres aléatoires c'est créer des nombres qui n'ont aucune connexion ou prédiction logique avec le nombre précédent. Les programmes qui nécessitent une certaine imprévisibilité ou des variations, comme les jeux ou les simulations, l'utilisent.

## Comment faire :

Voici comment générer un nombre aléatoire entre 1 et 100 en JavaScript:

```JavaScript 
let numAleatoire = Math.floor(Math.random() * 100) + 1;
console.log(numAleatoire);
```

Cela affichera un nombre aléatoire entre 1 et 100 chaque fois que vous exécutez le code.

## Plongée en profondeur 

Historiquement, générer des nombres vraiment aléatoires était un défi pour les premiers programmes informatiques. Aujourd'hui, JavaScript fournit une méthode intégrée `Math.random()` pour ce faire.

En termes d'alternatives, si vous avez besoin de nombres aléatoires suivant une certaine distribution (comme la distribution normale), vous devrez utiliser des bibliothèques supplémentaires ou écrire votre propre fonction.

Le `Math.random()` en JavaScript produit une suite de données pseudo-aléatoires et utilise un algorithme qui démarre à partir de la 'graine' initiale. Le changement de cette graine produira une pagination différente des nombres aléatoires. Sans graine spécifiée, JavaScript utilise la date et l'heure actuelles.

## Voir Aussi 

Here are some useful articles and references to experiment with random numbers:

1. [`Math.random()` MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
2. [Generating Random Numbers in JavaScript](https://medium.com/@josephcardillo/generating-random-numbers-in-javascript-1173526d498b)
3. [JavaScript Random game-projects](https://www.w3schools.com/js/js_random.asp)