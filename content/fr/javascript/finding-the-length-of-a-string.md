---
title:    "Javascript: Trouver la longueur d'une chaîne de caractères"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

La programmation est un langage universel qui peut être utilisé pour résoudre une variété de problèmes. Nous sommes constamment à la recherche de moyens d'optimiser et de simplifier notre code. Une des tâches les plus courantes en programmation est de trouver la longueur d'une chaîne de caractères. Dans cet article, nous allons découvrir pourquoi il est important de connaître la longueur d'une chaîne et comment le faire en utilisant Javascript.

## Comment faire

Pour trouver la longueur d'une chaîne en Javascript, nous pouvons utiliser la méthode `length`. Cette méthode renvoie le nombre de caractères dans une chaîne. Voici un exemple de code :

```Javascript
let str = "Bonjour!";
console.log(str.length); // Output: 8
```
Dans cet exemple, nous avons défini une variable `str` contenant la chaîne "Bonjour!" et nous avons utilisé la méthode `length` pour trouver sa longueur.

Nous pouvons également utiliser cette méthode pour trouver la longueur d'une chaîne contenue dans une variable :

```Javascript
let name = "Marie";
console.log(name.length); // Output: 5
```
La méthode `length` peut être utile pour vérifier la validité des entrées de l'utilisateur, pour obtenir la taille d'une liste ou d'un tableau ou pour créer un algorithme qui dépend de la longueur d'une chaîne.

## Deep Dive

Il est important de noter que la méthode `length` renvoie le nombre de caractères dans une chaîne et non le nombre de mots. Par exemple, si nous utilisons cette méthode sur la chaîne "Bonjour le monde", le résultat sera 15 car il y a 15 caractères, y compris les espaces.

De plus, il est important de comprendre que les espaces et les caractères spéciaux comptent également comme des caractères. Par conséquent, si nous avons une chaîne contenant des espaces et des caractères spéciaux, la méthode `length` prendra en compte tous les caractères et renverra leur somme.

## Voir aussi

- [Documentation Javascript sur la méthode `length`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length)
- [Lire et écrire des lignes de code en Javascript](https://www.w3schools.com/js/)
- [Exemples pratiques d'utilisation de la méthode `length`](https://www.geeksforgeeks.org/javascript-course-length-of-a-string/)