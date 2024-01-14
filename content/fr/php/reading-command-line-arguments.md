---
title:                "PHP: Lecture des arguments de ligne de commande"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez en PHP, il peut arriver que vous ayez besoin de passer des arguments en ligne de commande. Cela peut être utile pour personnaliser l'exécution de votre script en fonction de certains paramètres. Dans cet article, nous allons voir comment lire ces arguments en ligne de commande en PHP et comment les utiliser dans votre code.

## Comment faire

Pour lire les arguments en ligne de commande en PHP, vous pouvez utiliser la fonction `getopt()`. Elle prend deux arguments : une chaîne de caractères définissant les options à attendre, et un tableau contenant les arguments passés en ligne de commande. Voyons un exemple concret :

```PHP
// Script PHP pour lire les arguments en ligne de commande

// Définition des options à attendre
$options = "hn:f:";

// Récupération des arguments en ligne de commande dans un tableau
$args = getopt($options);

// Affichage du contenu du tableau $args
var_dump($args);
```

Si on exécute ce script avec la commande `php script.php -n "John" -f "Doe"`, on obtient le résultat suivant :

```
array(2) {
  ["n"]=>
  string(4) "John"
  ["f"]=>
  string(3) "Doe"
}
```

On remarque que les arguments passés en ligne de commande ont été stockés dans un tableau associatif, avec pour clé le nom de l'option et pour valeur la valeur correspondante. Ainsi, dans notre exemple, `-n` correspond au nom et `-f` au prénom.

Vous pouvez également spécifier des options qui attendent une valeur en ajoutant un `:` après le nom de l'option dans la chaîne de caractères passée en premier argument à `getopt()`. Par exemple, si on ajoute `i:` à notre chaîne d'options, cela signifie qu'on attend une valeur pour l'option `-i`. Voici un exemple :

```PHP
// Définition des options à attendre
$options = "hn:f:i:";

// Récupération des arguments en ligne de commande dans un tableau
$args = getopt($options);

// Affichage du contenu du tableau $args
var_dump($args);
```

Si on exécute ce script avec la commande `php script.php -n "John" -f "Doe" -i 123`, on obtient le résultat suivant :

```
array(3) {
  ["n"]=>
  string(4) "John"
  ["f"]=>
  string(3) "Doe"
  ["i"]=>
  string(3) "123"
}
```

On remarque que la valeur `123` a été associée à l'option `-i`.

## Deep Dive

En utilisant la fonction `getopt()`, vous pouvez également gérer les erreurs et les options non définies. Pour cela, il suffit de capturer la valeur de retour de `getopt()`. Si cette valeur est `false`, cela signifie que l'utilisateur a passé des options non valides. Vous pouvez alors afficher un message d'erreur et arrêter l'exécution de votre script.

De plus, si vous souhaitez passer plusieurs arguments pour une même option, vous pouvez les séparer par des virgules. Par exemple, si on modifie notre premier exemple et qu'on utilise la commande `php script.php -n "John,Doe"`, on obtient le résultat suivant :

```
array(1) {
  ["n"]=>
  array(2) {
    [0]=>
    string(4) "John"
    [1]=>
    string(3) "Doe"
  }
}
```

## Voir aussi

Références utiles pour approfondir vos connaissances sur la lecture des arguments en ligne de commande en PHP :

- [Documentation de la fonction `getopt()`](https://www.php.net/manual/fr/function.getopt.php)
- [Article sur la manipulation des arguments en ligne de commande en PHP](https://www.php.net/manual/fr/features.commandline.php)