---
title:                "Lecture des arguments de ligne de commande"
html_title:           "PHP: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur PHP, il est fort probable que vous ayez déjà utilisé la ligne de commande pour exécuter votre code. Mais saviez-vous que vous pouvez également utiliser la ligne de commande pour transmettre des arguments à votre script PHP ? Dans cet article, nous allons explorer pourquoi et comment lire simplement des arguments de ligne de commande en PHP.

## Comment faire

Pour lire les arguments de ligne de commande en PHP, nous allons utiliser la fonction `getopt()`. Cette fonction prend deux paramètres optionnels : les options à fournir et les arguments à lire. Considérons cet exemple de code :

```PHP
<?php
$options = getopt("a:b:c:d:");
print_r($options);
```

Lorsqu'on exécute ce script en ligne de commande avec des options d'arguments, par exemple `php script.php -a value1 -b value2`, nous obtenons une sortie similaire à ceci :

```
Array
(
    [a] => value1
    [b] => value2
)
```

Comme vous pouvez le voir, la fonction `getopt()` nous retourne un tableau associatif contenant les options et leurs valeurs fournies en ligne de commande.

Nous pouvons également utiliser des arguments positionnels en passant un tableau contenant les noms de ces arguments comme deuxième paramètre de la fonction `getopt()`. Par exemple :

```PHP
<?php
$arguments = array("argument1", "argument2");
$options = getopt("", $arguments);
print_r($options);
```

En exécutant ce script avec des arguments de ligne de commande, par exemple `php script.php value1 value2`, nous obtenons une sortie similaire à ceci :

```
Array
(
    [argument1] => value1
    [argument2] => value2
)
```

Vous pouvez également fournir des options avec des valeurs requises en utilisant un double-point (`:`) après le nom de l'option. Par exemple, `getopt("a:", $arguments)` attendra une valeur pour l'option `a`.

La fonction `getopt()` retourne `false` si aucune option n'a été fournie en ligne de commande.

## Deep Dive

La lecture des arguments de ligne de commande peut être très utile dans différentes situations, par exemple pour obtenir des valeurs de configuration ou pour créer des scripts flexibles et personnalisables. Vous pouvez également utiliser la fonction `getopt()` pour valider les arguments fournis en vérifiant s'ils correspondent aux options attendues.

En plus de la fonction `getopt()`, il existe d'autres façons de lire les arguments de ligne de commande en PHP. Par exemple, la variable `$argv` est un tableau contenant tous les arguments passés en ligne de commande, y compris le nom du script lui-même. Vous pouvez également utiliser la fonction `$_SERVER['argv']` pour obtenir les mêmes informations.

## Voir aussi

- [Documentation sur la fonction getopt() en PHP](https://www.php.net/manual/fr/function.getopt.php)
- [Tutoriel sur la lecture des arguments de ligne de commande en PHP](https://www.codecourse.com/forum/topics/php-reading-command-line-arguments)