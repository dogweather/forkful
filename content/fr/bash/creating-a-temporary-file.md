---
title:                "Création d'un fichier temporaire"
html_title:           "Bash: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une étape courante lors de la programmation en Bash. Que ce soit pour stocker des données temporaires ou pour créer un emplacement de travail, les fichiers temporaires sont indispensables pour de nombreuses tâches courantes en programmation. Dans cet article, nous allons vous expliquer pourquoi et comment créer des fichiers temporaires en Bash.

## Comment faire

Pour créer un fichier temporaire en Bash, vous pouvez utiliser la commande `mktemp`. Cette commande va créer un fichier temporaire dans le répertoire spécifié et afficher le chemin vers ce fichier. Exemple:

```Bash
$ mktemp -t mytempfile
/tmp/mytempfile
```

Vous pouvez également spécifier un préfixe pour le nom de fichier temporaire en utilisant l'option `-p`. Exemple:

```Bash
$ mktemp -p /home/user/Desktop/ -t mytempfile
/home/user/Desktop/mytempfile
```

Une fois que vous avez créé votre fichier temporaire, vous pouvez y écrire des données en utilisant la commande `echo`. Exemple:

```Bash
$ echo "Hello World!" > /tmp/mytempfile
```

Enfin, n'oubliez pas de supprimer votre fichier temporaire une fois que vous avez terminé en utilisant la commande `rm`. Exemple:

```Bash
$ rm /tmp/mytempfile
```

## Plongée en profondeur

Il est important de noter que les fichiers temporaires créés avec la commande `mktemp` ne seront pas automatiquement supprimés une fois que votre script Bash se termine. Il est donc recommandé de les supprimer manuellement à la fin de votre script. Vous pouvez également utiliser la commande `trap` pour supprimer automatiquement votre fichier temporaire en cas d'erreur dans votre script.

De plus, vous pouvez utiliser les options `-d` ou `-u` avec la commande `mktemp` pour créer un répertoire temporaire ou un fichier qui est automatiquement supprimé lorsque vous quittez votre session.

## Voir aussi

- [Documentation sur la commande `mktemp`](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Tutoriel pour créer des fichiers temporaires en Bash](https://www.linuxjournal.com/content/bash-temporary-files)
- [Exemples pratiques d'utilisation des fichiers temporaires en Bash](https://opensource.com/article/18/11/working-temporary-files-linux)