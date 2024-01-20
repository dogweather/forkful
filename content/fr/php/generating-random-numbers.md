---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Générer des nombres aléatoires, c'est simplement créer des nombres qui ne suivent aucune séquence prévisible. Les programmeurs le font pour diverses raisons, y compris pour effectuer des simulations, des tests de stress, de la cryptographie, ou pour rendre les jeux plus intéressants.

## Comment Faire:

Voici comment générer des nombres aléatoires en PHP:

```PHP
<?php   
   // générer un nombre aléatoire
   $randomNumber = rand();
   echo $randomNumber;

   // générer un nombre aléatoire entre 10 et 30
   $randomNumberBetween = rand(10, 30);
   echo $randomNumberBetween;   
?>  
```

Dans le code ci-dessus, la fonction  `rand()` génère des nombres aléatoires. Si vous fournissez les arguments `min` et `max`, cela générera un nombre aléatoire entre ces deux valeurs, inclusivement.

## Dive Profond:

Historiquement, la première fonction en PHP pour générer des nombres aléatoires était `rand()`. Cependant, elle n'a pas été conçue pour générer des nombres avec une bonne distribution aléatoire, mais seulement pour être rapide et légère.

Une alternative sûre est l'utilisation de la fonction `random_int()`, introduite en PHP 7, qui génère un nombre entier aléatoire cryptographiquement sûr entre deux limites données, inclusivement.

Pour plus de détails sur l'implémentation, consultez les documents officiels PHP sur [`rand()`](https://www.php.net/manual/fr/function.rand.php) et [`random_int()`](https://www.php.net/manual/fr/function.random-int.php).

## Voir Aussi:

Pour plus d'informations sur les nombres aléatoires en PHP, consultez ces ressources:

- [Fonctions de génération de nombres aléatoires en PHP](https://www.php.net/manual/fr/ref.math.php)
- [random_int() vs rand()](https://www.php.net/manual/fr/function.random-int.php)
- [Génération de nombres aléatoires sécurisés](https://paragonie.com/blog/2015/07/how-safely-generate-random-strings-and-integers-in-php)