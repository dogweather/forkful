---
title:                "Fish Shell: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

La lecture des arguments de ligne de commande est une compétence importante pour tout programmeur. Cela permet de gérer facilement et efficacement les entrées de l'utilisateur, ce qui est essentiel dans de nombreux projets de programmation.

## Comment faire

Pour lire les arguments de ligne de commande dans Fish Shell, nous pouvons utiliser la commande intégrée `set -x`. Voici un exemple de code et sa sortie :

```
Fish Shell $ set -x arg1 arg2 arg3
```

```Fish Shell 
\x arg1 
\x arg2 
\x arg3
```

Comme vous pouvez le voir, la commande `set -x` sépare les arguments par des lignes commençant par `\x`, ce qui rend la lecture des arguments très facile et claire.

## Plongée en profondeur

Il est important de noter que la commande `set -x` permet également de lire les arguments sous forme de tableau en utilisant l'option `-a`. Cela peut être utile dans certaines situations pour manipuler les arguments de manière plus avancée.

Nous pouvons également utiliser la commande `count` pour connaître le nombre d'arguments passés à notre script. Voici un exemple :

```Fish Shell
Fish Shell $ count $argv
```

```Fish Shell
3
```

En utilisant cette commande, nous pouvons facilement vérifier si le bon nombre d'arguments a été passé à notre script et prendre les mesures appropriées en cas de nombre incorrect.

# Voir aussi

Voici quelques ressources utiles pour en savoir plus sur la lecture des arguments de ligne de commande en Fish Shell :

- Documentation officielle de Fish Shell pour la commande `set` : [lien](https://fishshell.com/docs/current/cmds/set.html)
- Tutoriel sur la lecture des arguments de ligne de commande en Fish Shell : [lien](https://medium.com/@alexanderstepanov/how-to-write-bash-shell-scripts-faster-with-less-effort-682282b725f3)