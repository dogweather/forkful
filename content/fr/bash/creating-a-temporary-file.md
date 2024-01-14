---
title:    "Bash: Création d'un fichier temporaire"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Bash, vous avez probablement déjà rencontré des situations où vous aviez besoin de stocker temporairement des données pour une utilisation ultérieure. C'est là que la création de fichiers temporaires entre en jeu. Ces fichiers peuvent être utilisés pour stocker des données temporaires telles que des résultats intermédiaires ou des fichiers temporaires pour les scripts. Dans cet article, nous allons plonger dans la création de fichiers temporaires en Bash et vous montrer comment cela peut être utile dans votre code.

## Comment faire

La création d'un fichier temporaire en Bash est assez simple. Il suffit d'utiliser la commande `mktemp`. Cette commande créera un fichier avec un nom unique dans le répertoire temporaire par défaut du système. La syntaxe de base est la suivante :

```Bash
mktemp
```

Cela créera un fichier temporaire avec un nom unique commençant par `tmp` dans le répertoire `/tmp`. Cependant, vous pouvez également spécifier un préfixe pour le nom de fichier en utilisant l'option `-p`. Par exemple, si vous voulez que votre fichier temporaire commence par `file`, vous pouvez utiliser la commande suivante :

```Bash
mktemp -p file
```

Vous pouvez également spécifier un suffixe en utilisant l'option `-t`. Par exemple, si vous voulez que votre fichier temporaire se termine par `.txt`, vous pouvez utiliser la commande suivante :

```Bash
mktemp -t .txt
```

Vous pouvez également spécifier à la fois un préfixe et un suffixe :

```Bash
mktemp -p file -t .txt
```

Le résultat sera un nom de fichier unique commençant par `file` et se terminant par `.txt`.

Enfin, si vous avez besoin de créer plusieurs fichiers temporaires en une seule fois, vous pouvez utiliser l'option `-u` pour spécifier le nombre de fichiers à créer :

```Bash
mktemp -u -p file -t .txt
```

Voici un exemple de sortie incluant ces options :

```Bash
$ mktemp -u -p file -t .txt
fileHNvSTrDA.txt
fileOimSLuSO.txt
filepljwCLbV.txt
```

## Plongée en profondeur

Maintenant que nous avons vu comment créer un fichier temporaire en Bash, allons un peu plus en profondeur. Les fichiers temporaires sont utiles car ils sont créés rapidement et peuvent être utilisés pour stocker des données intermédiaires sans occuper d'espace mémoire. Cela peut être particulièrement utile lorsque vous travaillez avec des fichiers volumineux.

De plus, les fichiers temporaires sont également automatiquement supprimés lorsque le script se termine ou lorsque le fichier est fermé. Cela signifie que vous n'avez pas à vous soucier de nettoyer manuellement les fichiers temporaires après leur utilisation.

Vous pouvez également spécifier l'emplacement du répertoire temporaire en utilisant la variable `TMPDIR`. Si vous voulez que vos fichiers temporaires soient stockés dans un répertoire spécifique, vous pouvez définir cette variable dans votre script Bash.

Enfin, si vous avez besoin de stocker des données plus complexes, vous pouvez également utiliser des fichiers temporaires comme sockets Unix, qui peuvent être utilisés pour la communication entre des processus.

## Voir aussi

- [Guide Avancé Bash](https://www.gnu.org/software/bash/manual/)
- [Commande mktemp](https://www.man7.org/linux/man-pages/man3/mktemp.3.html)
- [Variables Shell Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)