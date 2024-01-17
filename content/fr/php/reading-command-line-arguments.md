---
title:                "Lecture des arguments en ligne de commande"
html_title:           "PHP: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La lecture des arguments de ligne de commande est une fonctionnalité importante dans la programmation PHP. Elle permet aux programmeurs d'interagir avec leur programme en fournissant des paramètres spécifiques lors de son exécution. Cela peut être utile pour personnaliser le comportement du programme en fonction des besoins de l'utilisateur.

## Comment le faire:
```PHP
<?php
// Syntaxe pour récupérer les arguments de ligne de commande
$arguments = getopt("l:f:");

// Exemple d'utilisation: Affiche les arguments spécifiés lors de l'exécution
echo "Langue: ".$arguments['l']."\n";
echo "Fichier: ".$arguments['f']."\n";
?>
```

Il est également possible de récupérer tous les arguments de la ligne de commande en utilisant la variable `$argc` et la fonction `argv()`. Voici un exemple:

```PHP
<?php
// Récupère le nombre d'arguments
$arguments_count = $argc;

// Parcourt tous les arguments et les affiche
for($i = 0; $i < $arguments_count; $i++) {
  echo "Argument $i : ".$argv[$i]."\n";
}
?>
```

Lors de l'exécution du programme, vous pouvez spécifier les arguments en utilisant le format suivant:

```bash
php mon_programme.php -l fr -f fichier.txt
```

Le résultat affichera alors:

```bash
Langue: fr
Fichier: fichier.txt
```

## Plongée en profondeur:
La lecture des arguments de ligne de commande est une fonctionnalité qui existe depuis longtemps dans les langages de programmation et qui est utilisée dans de nombreux environnements, tels que les scripts shell et les applications en ligne de commande. En plus de l'utiliser pour personnaliser le comportement du programme, il peut également être utile pour le débogage et le test.

En plus de la fonction `getopt()`, il existe également une fonction `getopt_long()` qui permet de spécifier des options à plusieurs caractères pour les arguments. Il est également possible de définir des options facultatives et des valeurs par défaut pour les arguments.

## Voir aussi:
- [Documentation officielle de PHP sur la lecture des arguments de ligne de commande](https://www.php.net/manual/fr/features.commandline.arguments.php)
- [Un tutoriel sur la lecture des arguments de ligne de commande en PHP](https://www.phpkitchen.com/2012/06/reading-command-line-arguments-from-php-cli/)
- [Un autre tutoriel sur la lecture des arguments de ligne de commande en PHP](https://www.youtube.com/watch?v=Fizs2jMNQg8)