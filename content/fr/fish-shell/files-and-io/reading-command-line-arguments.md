---
title:                "Lecture des arguments de ligne de commande"
aliases:
- /fr/fish-shell/reading-command-line-arguments.md
date:                  2024-01-20T17:55:49.759452-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Lire les arguments de la ligne de commande, ça consiste à récupérer ce que l'utilisateur tape après le nom de ton script. On fait ça pour rendre nos scripts flexibles et interactifs.

## How to:
```Fish Shell
# Script: greet.fish
for arg in $argv
    echo "Salut, $arg !"
end
```
Exécution et sortie :
```
> fish greet.fish Monde Utilisateur
Salut, Monde !
Salut, Utilisateur !
```
Utilisez `$argv` pour accéder aux arguments. C'est simple et efficace.

## Deep Dive
Historiquement, les shells ont toujours permis de passer des arguments pour des scripts shell, et Fish suit cette tradition. En comparaison, les anciens shells comme Bash utilisent `$1`, `$2`, etc., pour accéder aux arguments; Fish a choisi un tableau `$argv` pour plus de clarté. Côté implémentation, Fish gère les arguments comme des chaînes de caractères dans le tableau `$argv`, qu'on peut ensuite manipuler comme on veut avec les commandes et fonctions Fish.

## See Also
- Documentation officielle de Fish sur les variables spéciales : [fishshell.com/docs/current/index.html#variables-special](https://fishshell.com/docs/current/index.html#variables-special)
- Tutoriel Fish pour débutants : [learnxinyminutes.com/docs/fish/](https://learnxinyminutes.com/docs/fish/)
