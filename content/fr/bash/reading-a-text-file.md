---
title:                "Bash: Lecture d'un fichier texte"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire et écrire des fichiers texte est une compétence fondamentale en programmation Bash. En comprenant comment lire des fichiers texte, vous pourrez manipuler et gérer des données de manière efficace dans vos scripts Bash.

## Comment faire

Tout d'abord, vous devez vous assurer que vous avez les autorisations nécessaires pour lire le fichier texte en question. Vous pouvez vérifier cela en utilisant la commande `ls -l` dans votre terminal Bash.

Ensuite, vous pouvez utiliser la commande `cat` pour afficher le contenu d'un fichier texte directement dans votre terminal. Par exemple, si votre fichier est nommé "monfichier.txt", vous pouvez utiliser la commande suivante:

```Bash
cat monfichier.txt
```

Vous pouvez également utiliser la commande `less` pour naviguer à travers le contenu d'un fichier texte. La commande `less` vous permet de faire défiler le fichier ligne par ligne avec les touches fléchées ou les touches "Page haut" et "Page bas". Pour quitter la vue de `less`, vous pouvez appuyer sur la touche "Q" de votre clavier.

```Bash
less monfichier.txt
```

Pour plus de flexibilité, vous pouvez utiliser la commande `grep` pour rechercher des mots spécifiques dans un fichier texte. Par exemple, si vous souhaitez trouver toutes les occurrences du mot "Bonjour" dans votre fichier, vous pouvez utiliser la commande suivante:

```Bash
grep "Bonjour" monfichier.txt
```

## Plongée en profondeur

Maintenant que nous avons vu les commandes de base pour lire un fichier texte en utilisant Bash, il est également utile de comprendre comment ces commandes fonctionnent en interne.

Lorsque vous utilisez la commande `cat`, le contenu du fichier est simplement affiché à l'écran. Cependant, la commande `less` utilise un concept appelé "pagination" pour afficher correctement les informations. Elle utilise un tampon, un espace de stockage temporaire en mémoire, pour afficher le fichier ligne par ligne. Cela permet de ne pas afficher tout le contenu d'un fichier texte sur votre écran en même temps.

Quant à la commande `grep`, elle utilise des expressions régulières pour rechercher des motifs spécifiques dans un fichier. Les expressions régulières peuvent être assez complexes, donc si vous êtes intéressé à en savoir plus, il serait utile de se familiariser avec celles-ci.

## Voir aussi

- [Introduction à la ligne de commande Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners#h4-1-introduction-to-bash-syntax)
- [Documentation officielle de la commande `cat`](https://www.gnu.org/software/coreutils/manual/html_node/cat-invocation.html)
- [Documentation officielle de la commande `less`](https://linux.die.net/man/1/less)
- [Documentation officielle de la commande `grep`](https://www.gnu.org/software/grep/manual/grep.html)