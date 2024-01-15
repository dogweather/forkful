---
title:                "Génération de nombres aléatoires"
html_title:           "PHP: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires?

Générer des nombres aléatoires est une pratique courante en programmation, que ce soit pour des jeux, des simulations ou des tests. Les nombres aléatoires permettent de créer des scénarios différents à chaque exécution d'un programme, ce qui le rend plus dynamique et intéressant.

## Comment faire

La fonction `rand()` en PHP permet de générer des nombres aléatoires compris entre deux valeurs données. Voici un exemple d'utilisation :

```PHP
$randomNumber = rand(1, 10);
echo $randomNumber; // Affiche un nombre aléatoire entre 1 et 10
```

On peut également utiliser la fonction `mt_rand()` qui utilise un algorithme plus performant pour générer des nombres aléatoires. Voici un exemple d'utilisation :

```PHP
$randomNumber = mt_rand(100, 500);
echo $randomNumber; // Affiche un nombre aléatoire entre 100 et 500
```

Il est également possible de générer des nombres aléatoires avec des décimales en utilisant la fonction `mt_rand()` en combinaison avec la fonction `mt_getrandmax()` :

```PHP
$randomNumber = mt_rand(0, mt_getrandmax()) / mt_getrandmax();
echo $randomNumber; // Affiche un nombre aléatoire avec des décimales entre 0 et 1
```

## Plongée en profondeur

La génération de nombres aléatoires repose sur un algorithme qui utilise une graine (seed) pour déterminer quelle sera la prochaine valeur aléatoire. Si on utilise la même graine, on obtiendra toujours la même séquence de nombres aléatoires. C'est pourquoi il est important de choisir une graine différente à chaque exécution pour obtenir une séquence réellement aléatoire.

Il est également possible de spécifier une graine à utiliser avec les fonctions `rand()` et `mt_rand()` en ajoutant un troisième paramètre :

```PHP
$randomNumber = rand(1, 10, 42);
echo $randomNumber; // Affiche un nombre aléatoire entre 1 et 10 basé sur la graine 42
```

Enfin, il existe d'autres fonctions en PHP pour générer des nombres aléatoires, telles que `random_int()` pour générer des entiers cryptographiquement sûrs, ou `random_bytes()` pour générer des séquences d'octets aléatoires.

## Voir aussi

- [Documentation officielle de PHP sur les fonctions de génération de nombres aléatoires](https://www.php.net/manual/fr/function.rand.php)
- [Article sur la génération de nombres aléatoires en JavaScript](https://www.freecodecamp.org/news/how-to-create-a-random-number-generator-in-javascript/)