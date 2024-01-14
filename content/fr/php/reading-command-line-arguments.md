---
title:    "PHP: Lecture des arguments de la ligne de commande."
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Pourquoi
Si vous êtes un développeur débutant en PHP, vous avez probablement entendu parler de la lecture des arguments de ligne de commande (CLI). Mais saviez-vous que c'est en fait une compétence très utile à avoir en tant que programmeur PHP ? Dans cet article, nous allons explorer pourquoi la lecture des arguments de ligne de commande est importante et comment vous pouvez l'utiliser dans vos projets.

## Comment Faire
Pour lire les arguments de ligne de commande en PHP, nous allons utiliser la fonction `getopt()`. Cette fonction prend en paramètre une chaîne contenant toutes les options possibles pour votre script et retourne un tableau contenant les arguments fournis par l'utilisateur. Voyons un exemple concret. 

```PHP
<?php
// Script qui prend deux arguments : -n pour nom et -a pour âge
$options = "n:a:";
$commandLineArgs = getopt($options);
print_r($commandLineArgs);
```
Si nous exécutons ce script avec `php script.php -n Jean -a 25`, le résultat sera :

```PHP
Array
(
    [n] => Jean
    [a] => 25
)
```
Comme vous pouvez le voir, la fonction `getopt()` a retourné un tableau associatif, avec les clés correspondant aux options spécifiées et les valeurs correspondant aux arguments fournis. Vous pouvez ensuite utiliser ces valeurs dans votre script pour effectuer différentes actions en fonction des options choisies par l'utilisateur. 

## Plongée En Profondeur
Il est important de comprendre que la lecture des arguments de ligne de commande n'est pas limitée à simplement récupérer des valeurs. Vous pouvez également utiliser des options comme des indicateurs pour activer ou désactiver certaines fonctionnalités de votre script. Par exemple, vous pouvez utiliser l'option `-v` pour activer un mode débogage qui affichera plus d'informations sur l'exécution de votre script. 

De plus, il est possible d'ajouter des valeurs par défaut pour vos options en utilisant deux points après le nom de l'option, par exemple `-n:` spécifie que l'option `-n` doit être suivie d'une valeur. Si aucune valeur n'est fournie par l'utilisateur, la valeur par défaut sera utilisée à la place. 

## Voir Aussi
- [Documentation officielle de PHP sur la fonction `getopt()`](https://www.php.net/manual/en/function.getopt.php)
- [Tutoriel sur les arguments de ligne de commande en PHP](https://dev.to/mattsparks/phps-command-line-argument-handling-with-getopt-217a)
- [Exemples pratiques de la lecture des arguments de ligne de commande en PHP](https://www.programiz.com/php-programming/command-line-arguments)