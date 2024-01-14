---
title:    "PHP: La lecture des arguments en ligne de commande"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi
Les arguments de ligne de commande sont un aspect important de la programmation en PHP, car ils permettent d'interagir avec le programme en lui fournissant des instructions spécifiques. Savoir comment lire ces arguments peut être très utile pour les développeurs lorsqu'ils travaillent sur des projets ou des scripts en ligne de commande.

## Comment le faire
Pour lire les arguments de ligne de commande en PHP, nous pouvons utiliser la fonction `getopt()`. Elle prend en paramètre les arguments attendus et retourne un tableau contenant les valeurs passées en ligne de commande. Voici un exemple de code :

```PHP
<?php
$options = getopt("a:b:"); // Définir les arguments attendus
echo $options['a']; // Afficher la valeur passée pour l'argument a
echo $options['b']; // Afficher la valeur passée pour l'argument b
```

Si nous exécutons ce code avec les arguments `php script.php -a valeurA -b valeurB`, nous obtiendrons l'affichage suivant :

```
valeurA
valeurB
```

Nous pouvons également utiliser la variable `$argc` pour obtenir le nombre d'arguments passés en ligne de commande, et la variable `$argv` pour accéder à ces arguments individuellement.

## Plongée en profondeur
Il est important de noter que l'ordre dans lequel les arguments sont passés importe. Si nous utilisions l'exemple précédent et échangions les valeurs de `a` et `b`, nous obtiendrions un résultat différent. De plus, si un argument est défini avec deux points après le nom (par exemple `a::`), cela signifie qu'il est facultatif.

Il existe également des options avancées pour la fonction `getopt()`, telles que la gestion des erreurs et la prise en charge des options à plusieurs caractères.

## Voir aussi
- [Documentation officielle sur la fonction `getopt()` en français](https://www.php.net/manual/fr/function.getopt.php)
- [Tutoriel sur les arguments de ligne de commande en PHP](https://www.tutorialspoint.com/php/php_command_line.htm)
- [Article sur l'utilisation de `getopt()` pour créer des interfaces en ligne de commande en PHP](https://www.phpflow.com/php/command-line-arguments-in-php/)