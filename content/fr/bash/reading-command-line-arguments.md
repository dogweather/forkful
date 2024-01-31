---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:55:23.137244-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Lire les arguments de la ligne de commande, c'est récupérer les données fournies lors du lancement d'un script Bash. Les programmeurs utilisent cette technique pour rendre leurs scripts flexibles et interactifs.

## How to:
Voici un exemple simple. Le script `hello.sh` salue l'utilisateur en utilisant l'argument fourni.

```Bash
#!/bin/bash
echo "Bonjour, $1 !"
```

Lancer le script avec `./hello.sh Monde` affiche:

```
Bonjour, Monde !
```

Pour traiter plusieurs arguments:

```Bash
#!/bin/bash
echo "Bonjour, $1 !"
echo "Comment ça va, $2 ?"
```

Avec `./hello.sh Monde Univers`, on obtient:

```
Bonjour, Monde !
Comment ça va, Univers ?
```

## Deep Dive
Historiquement, les scripts Bash sont pensés pour être des petits programmes non interactifs ou des automatisations de commandes répétitives. Lire les arguments de ligne de commande a transformé ces scripts en outils bien plus versatiles.

En alternative, `getopts` ou `optarg` sont utilisés pour des arguments plus complexes. Ces fonctions prennent en charge des options avec ou sans valeurs. Par exemple, `-v` pour verbose ou `-f fichier` pour spécifier un fichier.

Pour l'implémentation, `$1`, `$2`, etc., représentent les arguments positionnels. `$0` est le nom du script. `$#` indique le nombre d'arguments. `$@` les liste tous, ce qui est utile dans les boucles.

## See Also
- La page du manuel Bash pour `bash(1)` : `man bash`
- Guide avancé de scripting Bash : https://www.tldp.org/LDP/abs/html/
- Utilisation de `getopts` : https://wiki.bash-hackers.org/howto/getopts_tutorial
