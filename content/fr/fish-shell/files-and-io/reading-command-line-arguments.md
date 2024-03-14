---
date: 2024-01-20 17:55:49.759452-07:00
description: "Lire les arguments de la ligne de commande, \xE7a consiste \xE0 r\xE9\
  cup\xE9rer ce que l'utilisateur tape apr\xE8s le nom de ton script. On fait \xE7\
  a pour rendre nos\u2026"
lastmod: '2024-03-13T22:44:58.341127-06:00'
model: gpt-4-1106-preview
summary: "Lire les arguments de la ligne de commande, \xE7a consiste \xE0 r\xE9cup\xE9\
  rer ce que l'utilisateur tape apr\xE8s le nom de ton script. On fait \xE7a pour\
  \ rendre nos\u2026"
title: Lecture des arguments de ligne de commande
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
