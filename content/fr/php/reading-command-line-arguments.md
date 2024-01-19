---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Lire des arguments de ligne de commande en PHP, c'est simplement récupérer des valeurs transmises à un script via l'interface de ligne de commande. Les développeurs l'utilisent pour passer des paramètres à un script lors de son exécution.

## Comment faire:

Voici un exemple simple montrant comment lire des arguments de ligne de commande en PHP:

```PHP
<?php
// Vérifiez s'il y a des arguments
if($argc > 1) {
    // Parcourir les arguments (sauf le nom du script)
    for($i = 1; $i < $argc; $i++)
        echo "Argument $i: $argv[$i]\n";
}
?>
```
Si vous exécutez ce script avec des arguments, par exemple `php script.php arg1 arg2`, votre sortie serait:

```
Argument 1: arg1
Argument 2: arg2
```

## Plongée Profonde

Historiquement, la lecture des arguments de ligne de commande existe depuis les premières journées de la programmation. Cela permet une interaction dynamique avec des scripts ou des programmes.

En alternative à `$argv` et `$argc`, vous pouvez utiliser `getopt()` pour lire des arguments formatés de manière plus complexe. Par exemple:

```PHP
<?php
$options = getopt("a:b:c:");
print_r($options);
?>
```

Un aspect intéressant de la mise en œuvre en PHP est que le premier argument de `$argv` est toujours le nom du script lui-même (comparable à d'autres langages).

## Voir Aussi

* Documentation PHP sur les arguments de ligne de commande: https://www.php.net/manual/fr/reserved.variables.argv.php
* Fonction PHP `getopt()`: https://www.php.net/manual/fr/function.getopt.php

N'oubliez pas que la pratique conduit à la perfection! Continuez donc à coder.