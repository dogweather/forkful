---
title:                "PHP: Production de nombres aléatoires"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi 

La génération de nombres aléatoires est un aspect essentiel de la programmation en PHP. Elle permet de créer des applications plus dynamiques, interactives et sécurisées. Dans cet article, nous allons explorer en détails comment générer des nombres aléatoires en PHP.

## Comment faire

La méthode la plus simple pour générer des nombres aléatoires en PHP est d'utiliser la fonction `rand()` qui prend en paramètres un nombre minimum et un nombre maximum et retourne un nombre aléatoire compris entre ces deux valeurs.

```PHP
<?php 
// Générer un nombre aléatoire entre 1 et 10
$nombre = rand(1,10);
echo $nombre; // Output: un nombre aléatoire entre 1 et 10
?>
```

Il est également possible d'utiliser la fonction `mt_rand()` qui est plus rapide et plus sécurisée que `rand()`.

```PHP
<?php 
// Générer un nombre aléatoire entre 1 et 100
$nombre = mt_rand(1,100);
echo $nombre; // Output: un nombre aléatoire entre 1 et 100
?>
```

Si vous voulez générer un nombre aléatoire à virgule, vous pouvez utiliser la fonction `mt_rand()` combinée avec la fonction `floatval()`.

```PHP
<?php 
// Générer un nombre aléatoire à virgule entre 1 et 2
$nombre = floatval(mt_rand(1,100)/100);
echo $nombre; // Output: un nombre aléatoire à virgule entre 1 et 2
?>
```

## Plongée en profondeur

Il est important de noter que les nombres aléatoires générés par PHP ne sont pas vraiment aléatoires, mais plutôt pseudo-aléatoires. Cela signifie que ces nombres sont basés sur un algorithme et une graine (seed) qui détermine le premier nombre généré. Par défaut, la graine est aléatoire à chaque fois que le script est exécuté, mais il est possible de la définir manuellement en utilisant la fonction `srand()`.

Il est également possible de générer des chaînes de caractères aléatoires en utilisant la fonction `uniqid()`.

```PHP
<?php 
// Générer une chaîne de caractères aléatoire de 10 caractères
$chaine = uniqid('',true);
echo $chaine; // Output: une chaîne de caractères aléatoire de 10 caractères
?>
```

Enfin, il est important de noter que les nombres aléatoires générés doivent être utilisés avec prudence, surtout lorsqu'il s'agit de sécuriser des applications. Il est recommandé d'utiliser des fonctions spécifiques pour générer des mots de passe aléatoires ou des tokens d'authentification.

## Voir aussi
- [Documentation officielle de PHP pour la fonction rand()](https://www.php.net/manual/fr/function.rand.php)
- [Documentation officielle de PHP pour la fonction uniqid()](https://www.php.net/manual/fr/function.uniqid.php)
- [Article de blog sur la sécurité et la génération de nombres aléatoires en PHP](https://www.smashingmagazine.com/2012/01/entropy-online-succeeding-in-uncertain-times/)