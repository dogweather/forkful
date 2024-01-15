---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Bash: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous intéresser à la lecture des arguments via la ligne de commande? Eh bien, cette compétence peut vous permettre de personnaliser et d'automatiser vos tâches quotidiennes sur votre ordinateur. Cela peut également vous aider à mieux comprendre le fonctionnement de votre système d'exploitation.

## Comment

La lecture des arguments via la ligne de commande se fait à l'aide de la variable spéciale "$@" qui contient tous les arguments passés au script Bash. Vous pouvez alors utiliser une boucle "for" pour parcourir et traiter chaque argument individuellement. Voici un exemple de code :

```Bash
#!/bin/bash
#Script qui lit tous les arguments et les affiche à l'écran

for argument in "$@"
do
  echo "Argument: $argument"
done
```

Si vous exécutez ce script avec la commande "bash script.sh arg1 arg2", vous obtiendrez l'output suivant :

```
Argument: arg1
Argument: arg2
```

Vous pouvez également utiliser la variable "$#" pour obtenir le nombre total d'arguments passés. Voici un exemple qui affiche un message différent en fonction du nombre d'arguments :

```Bash
#!/bin/bash
#Script qui gère différents cas selon le nombre d'arguments

if [ "$#" -eq 0 ]; then 
  echo "Aucun argument passé." 
elif [ "$#" -eq 1 ]; then
  echo "Un seul argument passé."
else 
  echo "Plusieurs arguments passés."
fi
```

Si vous exécutez ce script avec la commande "bash script.sh arg1", vous obtiendrez "Un seul argument passé." Si vous utilisez la commande "bash script.sh arg1 arg2 arg3", vous obtiendrez "Plusieurs arguments passés."

## Deep Dive

En plus de la variable spéciale "$@", il existe d'autres moyens de lire les arguments via la ligne de commande en utilisant des options spécifiques telles que "getopts" ou "shift". Ces méthodes peuvent être utiles pour gérer des arguments de façon plus complexe et avec plus de flexibilité. Vous pouvez également utiliser le script "getopt" pour vous aider à traiter les arguments de façon plus avancée.

## Voir aussi

- [Guide de la ligne de commande pour débutants](https://blog.storagecraft.com/fr/guide-de-la-ligne-de-commande-pour-les-debutants/)
- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Scripting Bash pour les débutants](https://www.linux.com/fr/news/quest-ce-que-bash-scripting et comment le faire/)