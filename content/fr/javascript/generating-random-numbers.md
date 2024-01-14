---
title:    "Javascript: Génération de nombres aléatoires"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une tâche courante en programmation, que ce soit pour créer des jeux, des simulations ou pour tester du code. Les nombres aléatoires ajoutent une dimension aléatoire à un programme, le rendant plus interactif et imprévisible.

## Comment faire

Pour générer des nombres aléatoires en Javascript, nous pouvons utiliser la méthode `Math.random()`. Cela renvoie un nombre décimal aléatoire compris entre 0 et 1. Nous pouvons ensuite l'utiliser pour créer des nombres entiers aléatoires en multipliant le résultat par la plage de valeurs souhaitée et en utilisant la fonction `Math.floor()` pour l'arrondir à l'entier inférieur.

```javascript
// Générer un nombre aléatoire entre 1 et 10
var randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Exemple de sortie : 8
```

Si nous voulons générer un nombre aléatoire dans une plage spécifique, nous pouvons utiliser la formule suivante :

```javascript
// Générer un nombre aléatoire entre une plage donnée
var min = 5; // Valeur minimale
var max = 15; // Valeur maximale
var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNumber); // Exemple de sortie : 11
```

Nous pouvons également utiliser la méthode `Math.random()` pour générer d'autres types de nombres aléatoires. Par exemple, pour générer un nombre aléatoire à virgule flottante entre 0 et 10, nous pouvons utiliser la formule suivante :

```javascript
// Générer un nombre aléatoire à virgule flottante entre 0 et 10
var randomNumber = Math.random() * 10;
console.log(randomNumber); // Exemple de sortie : 7.24
```

## Plongée en profondeur

Bien que la méthode `Math.random()` soit assez simple, elle n'est pas parfaite. En fait, si nous utilisons cette méthode pour générer un grand nombre de nombres aléatoires, nous pourrions remarquer certains schémas ou des répétitions apparaissant. Cela peut être dû à la façon dont les ordinateurs génèrent des nombres aléatoires, qui sont souvent basés sur des algorithmes déterministes.

Pour résoudre ce problème, nous pouvons utiliser des générateurs de nombres aléatoires plus sophistiqués tels que le générateur congruentiel linéaire (Linear Congruential Generator) ou le générateur de Mersenne Twister. Ces générateurs sont conçus pour produire des nombres plus aléatoires en utilisant des algorithmes plus complexes.

Il est important de noter qu'en utilisant ces générateurs plus avancés, nous ne sommes toujours pas en mesure de générer vraiment des nombres aléatoires, mais simplement des nombres pseudo-aléatoires. Cela signifie que si nous connaissions l'algorithme utilisé par le générateur, nous pourrions prédire les résultats à venir. Cependant, pour la plupart des cas d'utilisation en programmation, ces générateurs sont suffisamment aléatoires.

## Voir aussi

- [Documentation officielle sur `Math.random()` (en anglais)](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Math/random)
- [Article sur les générateurs de nombres aléatoires (en anglais)](https://medium.com/@kevincennis/mental-models-for-randomness-9a8d7cafa599)
- [Cryptographie et générateurs de nombres aléatoires (en anglais)](https://medium.com/swlh/practical-guide-to-cryptography-and-random-numbers-89b5ca3079ab)