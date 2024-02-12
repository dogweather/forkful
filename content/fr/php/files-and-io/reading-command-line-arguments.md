---
title:                "Lecture des arguments de ligne de commande"
aliases:
- /fr/php/reading-command-line-arguments.md
date:                  2024-01-20T17:56:28.532678-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Lire les arguments de ligne de commande, c'est récupérer les données fournies à votre script PHP lors de son exécution dans un terminal. On le fait pour permettre aux utilisateurs de personnaliser l'exécution du script, comme spécifier un fichier à traiter ou choisir une option de configuration.

## How to: (Comment faire : )
En PHP, les arguments de ligne de commande sont accessibles via le tableau `$argv`. Le script affiche le premier argument (après le nom du fichier script).

```PHP
<?php
if ($argc > 1) {
    echo "Premier argument: " . $argv[1] . "\n";
} else {
    echo "Aucun argument fourni.\n";
}
?>
```

Si vous exécutez `php script.php Bonjour`, ça affichera:

```
Premier argument: Bonjour
```

## Deep Dive (Plongée profonde)
Historiquement, `$argv` et `$argc` sont empruntés de C, où "argv" signifie "argument vector" et "argc" est le "argument count". Alternativement, vous pouvez utiliser des bibliothèques pour analyser les options de ligne de commande plus complexes, comme `getopt()`. Niveau implémentation, `$argv` est un tableau indexé contenant les arguments, `$argv[0]` est toujours le nom de votre script, et les indices suivants, les arguments passés.

## See Also (Voir Aussi)
- [Documentation PHP sur `$argv` et `$argc`](https://www.php.net/manual/fr/reserved.variables.argv.php)
- [Page man de `getopt`](https://www.php.net/manual/fr/function.getopt.php)
- [Cours sur la ligne de commande PHP](https://www.php.net/manual/fr/features.commandline.php)
