---
title:    "Bash: Création d'un fichier temporaire"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une pratique courante en programmation Bash. Les fichiers temporaires sont créés pour stocker des données temporaires nécessaires à l'exécution d'un script ou pour stocker des informations à des fins de débogage. Ils ne sont pas destinés à être conservés à long terme et sont généralement supprimés après avoir rempli leur objectif. Dans cet article, nous allons explorer comment créer des fichiers temporaires en Bash et pourquoi cette pratique est si utile.

## Comment faire

La création d'un fichier temporaire en Bash est relativement simple. Tout d'abord, nous devons utiliser la commande `mktemp` pour générer un nom de fichier unique pour notre fichier temporaire. Cela garantit qu'il n'y aura pas de conflits de noms de fichiers si plusieurs scripts tentent de créer des fichiers temporaires en même temps.

Ensuite, nous pouvons utiliser cette commande pour créer notre fichier temporaire :

```Bash
tempfile=$(mktemp)
```
Dans cet exemple, nous stockons le nom du fichier temporaire dans une variable `tempfile`. Nous pouvons également spécifier un préfixe pour le nom de fichier en utilisant l'option `-p` de `mktemp`, comme ceci :

```Bash
tempfile=$(mktemp -p prefix_)
```

Une fois que nous avons créé notre fichier temporaire, nous pouvons y écrire des données en utilisant la commande `echo` et en redirigeant la sortie vers le fichier temporaire :

```Bash
echo "Ceci est un fichier temporaire" > $tempfile
```

Nous pouvons également lire les données à partir du fichier temporaire en utilisant la commande `cat` :

```Bash
cat $tempfile
```

Une fois que nous avons terminé d'utiliser le fichier temporaire, nous pouvons le supprimer en utilisant la commande `rm` :

```Bash
rm $tempfile
```

Il est également possible de spécifier un répertoire de stockage pour les fichiers temporaires en utilisant l'option `-t` de `mktemp`. Cela peut être utile si vous souhaitez stocker tous vos fichiers temporaires au même endroit.

## Plongée plus profonde

Lorsque nous créons un fichier temporaire en Bash, il est important de noter que le fichier est en fait stocké dans le répertoire "/tmp". Cela peut être vu en utilisant la commande `ls`, qui montre tous les fichiers temporaires créés par l'utilisateur courant :

```Bash
ls /tmp
```

Nous pouvons également spécifier un patern de nom de fichier en utilisant l'option `-t` de `mktemp`. Cela peut être utile pour créer des fichiers temporaires de façon séquentielle, par exemple en ajoutant un numéro de version au nom de fichier à chaque nouvelle création.

Enfin, il est important de noter que les fichiers temporaires ne sont pas uniquement destinés à être utilisés par des scripts. Vous pouvez également créer des fichiers temporaires dans vos propres programmes en utilisant des appels système tels que `mkstemp()` ou `tmpfile()`.

## Voir aussi

Pour en savoir plus sur la création de fichiers temporaires en Bash, vous pouvez consulter les ressources suivantes (en anglais) :

- [GNU Coreutils - Creating Temporary Files](https://www.gnu.org/software/coreutils/manual/html_node/Creating-Temporary-Files.html)
- [Linux Documentation Project - Bash Guide for Beginners - Temporary Files](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [Stack Overflow - How do I create a temporary file in the same directory as the current script?](https://stackoverflow.com/questions/5344687/how-do-i-create-a-temp-file-in-same-directory-as-the-running-script/5344741#5344741)